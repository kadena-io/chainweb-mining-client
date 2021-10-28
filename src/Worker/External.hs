{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Worker.External
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Worker.External
( externalWorker
) where

import Control.Concurrent.Async
import Control.Monad.Catch

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import GHC.Generics

import System.Exit
import System.IO
import System.LogLevel
import qualified System.Process as P

-- internal modules

import Logger
import Worker

-- -------------------------------------------------------------------------- --
--  Exceptions

newtype ExternalWorkerException = ExternalWorkerException T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception ExternalWorkerException

-- -------------------------------------------------------------------------- --
--

-- | Run an external worker:
--
-- External workers is an external operating system process that is provided by
-- it's file system path.
--
-- It is invoked with
--
-- * the target as first parameter (hex bytes in little endian encoding),
--   followed by
-- * what ever extra arguments are configured by the user.
--
-- The work bytes are provided to stdin as raw bytes.
--
-- On finding a solution it is expected to print the nonce (encoded in hex in
-- big endian byte order) and to exit with an exit code of 0.
--
-- In case of an exit code other than zero any outputs are logged and discarded.
--
externalWorker
    :: Logger
    -> String
        -- ^ worker command
    -> Worker
externalWorker logger cmd _nonce target (Work work) =
    withLogTag logger "Worker" $ \workerLogger ->
        P.withCreateProcess workerProc (go workerLogger)
  where
    targetArg = T.unpack $ targetToText16 target

    workerProc = (P.shell $ cmd <> " " <> targetArg)
        { P.std_in = P.CreatePipe
        , P.std_out = P.CreatePipe
        , P.std_err = P.CreatePipe
        }

    go workerLogger (Just hstdin) (Just hstdout) (Just hstderr) ph = do
        B.hPut hstdin $ BS.fromShort work
        hClose hstdin
        writeLog logger Info "send command to external worker"
        writeLog logger Debug $ "external worker command: " <> T.pack (cmd <> " " <> targetArg)
        withAsync (B.hGetContents hstdout) $ \stdoutThread ->
            withAsync (errThread workerLogger hstderr) $ \stderrThread -> do
                code <- P.waitForProcess ph
                (outbytes, _) <- (,) <$> wait stdoutThread <*> wait stderrThread
                writeLog logger Info "received nonce for solved work from external worker"
                if code /= ExitSuccess
                  then
                    throwM $ ExternalWorkerException $
                        "External worker failed with code: " <> (T.pack . show) code
                            -- FIXME: handle non-printable characters
                  else do
                    nonceB16 <- case B8.splitWith isSpace outbytes of
                        [] -> throwM $ ExternalWorkerException $
                            "expected nonce from miner, got: " <> T.decodeUtf8 outbytes
                            -- FIXME: handle non-printable characters
                        (a:_) -> return a

                    -- reverse -- we want little-endian
                    case BA.convertFromBase BA.Base16 nonceB16 of
                        Left e -> throwM $ ExternalWorkerException $
                            "failed to decode nonce bytes: " <> (T.pack . show) e
                        Right bs
                            | B.length bs /= 8 ->
                                throwM $ ExternalWorkerException "process returned short nonce"
                            | otherwise -> return $ Work $!
                                BS.toShort $ B.take 278 (BS.fromShort work) <> B.reverse bs

    go _ _ _ _ _ = throwM $ ExternalWorkerException
        "impossible: process is opened with CreatePipe in/out/err"

    errThread l hstderr = next
      where
        next = hIsEOF hstderr >>= \case
            True -> return ()
            False -> do
                T.hGetLine hstderr >>= writeLog l Debug
                next

