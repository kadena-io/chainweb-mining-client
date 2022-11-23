{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Worker.CPU
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Worker.CPU
( cpuWorker
) where

import Crypto.Hash.IO

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Int
import Data.IORef
import qualified Data.Text as T
import Data.Word

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)

import GHC.Exts

import System.LogLevel

-- internal modules

import Logger
import Target
import Worker
import WorkerUtils
import Utils

-- -------------------------------------------------------------------------- --
-- Worker

-- | Single threaded CPU mining worker for Chainweb.
--
-- TODO: Check the chainweb version to make sure this function can handle the
-- respective version.
--
cpuWorker
  :: forall a
  . HashAlgorithm a
  => Logger
  -> Worker
cpuWorker logger orig@(Nonce o) trg work = do
    nonces <- newIORef 0
    !ctx <- hashMutableInit @a
    new <- BA.copy hbytes $ \buf ->
        allocaBytes (powSize :: Int) $ \pow -> do

            -- inner mining loop
            --
            let go1 0 n = return (Just n)
                go1 !i n@(Nonce nv) = do
                    -- Compute POW hash for the nonce
                    injectNonce_ n buf
                    hash ctx buf pow

                    -- check whether the nonce meets the target
                    fastCheckTarget trgWords (castPtr pow) >>= \case
                        True -> Nothing <$ writeIORef nonces (nv - o)
                        False -> go1 (i - 1) (Nonce $! nv + 1)

            -- outer loop
            -- Estimates how many iterations of the inner loop run in one second. It runs the inner loop
            -- that many times and injects an updated creation time in each cycle.
            let go0 :: Int -> Int64 {- microseconds -} -> Nonce -> IO ()
                go0 x t !n = do
                    injectTime_ t buf
                    go1 x n >>= \case
                        Nothing -> return ()
                        Just n' -> do
                            t' <- getCurrentTimeMicros
                            let td = t' - t
                                x' = round @Double (int x * 1000000 / int td) -- target 1 second
                            go0 x' t' n'

            -- Start outer mining loop
            t <- getCurrentTimeMicros
            go0 100000 t orig
    attempts <- readIORef nonces
    writeLog logger Info $ "Solved header with " <> T.pack (show attempts) <> " attempts."
    return (Work $ BS.toShort new)
  where
    !trgWords = targetToWords trg
    !hbytes = let (Work b) = work in BS.fromShort b

    bufSize :: Int
    !bufSize = B.length hbytes

    powSize :: Int
    !powSize = hashDigestSize @a undefined

    --  Compute POW hash
    hash :: MutableContext a -> Ptr Word8 -> Ptr Word8 -> IO ()
    hash ctx buf pow = do
        hashMutableReset ctx
        BA.withByteArray ctx $ \ctxPtr -> do
            hashInternalUpdate @a ctxPtr buf $ fromIntegral bufSize
            hashInternalFinalize ctxPtr $ castPtr pow
    {-# INLINE hash #-}

