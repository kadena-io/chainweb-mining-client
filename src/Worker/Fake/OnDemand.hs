{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Worker.Fake.OnDemand
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- On-Demand Mining Worker
--
-- A fake mining worker that is not actually doing any work. It returns the
-- work bytes unchanged after being sent an HTTP POST request to `/make-block`.
--

module Worker.Fake.OnDemand (withOnDemandWorker) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch

import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Function
import Data.Streaming.Network
import qualified Data.Text as T

import Network.HostAddress
import qualified Network.Wai as Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp

import qualified System.LogLevel as L

-- internal modules

import Logger
import Worker

withOnDemandWorker
    :: Logger
    -> Port
    -> HostPreference
    -> ((Logger -> Worker) -> IO ())
    -> IO ()
withOnDemandWorker logger port host inner = do
    mineSemaphore <- newEmptyMVar @ChainId
    race (server mineSemaphore) (inner (worker mineSemaphore)) >>= \case
        Left _ -> writeLog logger L.Error "server exited unexpectedly"
        Right _ -> do
            writeLog logger L.Error "mining loop existed unexpectedly"
            return ()
  where
    server mineSemaphore = flip finally (writeLog logger L.Info "server stopped") $
        Warp.runSettings setts $ \req resp ->
            if Wai.pathInfo req == ["make-block"] && Wai.requestMethod req == methodPost
            then tryTakeMVar mineSemaphore >>= \cid -> resp (done cid)
            else resp notFound
    notFound = Wai.responseLBS notFound404 [] "The on-demand worker's only endpoint is /make-block"
    done (Just cid) = Wai.responseLBS ok200 [] (LBS8.pack $ show cid)
    done Nothing = Wai.responseLBS serviceUnavailable503 [] "not enough work to make a block"
    setts = Warp.defaultSettings
        & Warp.setPort (fromIntegral port)
        & Warp.setHost host

worker :: MVar ChainId -> Logger -> Worker
worker mineSemaphore minerLogger _nonce _target cid work = do
    putMVar mineSemaphore cid
    writeLog minerLogger L.Info $ "block demanded for chain " <> T.pack (show cid)
    return work
