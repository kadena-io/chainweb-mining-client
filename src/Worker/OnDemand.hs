{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Worker.OnDemand
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- On-Demand Mining Worker
--
-- A mining worker that is not doing proof of work. It returns the work bytes
-- unchanged after being sent an HTTP POST request to `/make-block`.
--

module Worker.OnDemand (withOnDemandWorker) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Function
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Streaming.Network
import qualified Data.Text as T

import Network.HostAddress
import qualified Network.Wai as Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import Web.DeepRoute
import Web.DeepRoute.Wai

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
    miningGoals <- newTVarIO (HashMap.empty @ChainId @Word)
    race (server miningGoals) (inner (worker miningGoals)) >>= \case
        Left _ -> writeLog logger L.Error "server exited unexpectedly"
        Right _ -> writeLog logger L.Error "mining loop exited unexpectedly"
  where
    server goalsRef = flip finally (writeLog logger L.Info "server stopped") $
        Warp.runSettings setts $ \req resp -> routeWaiApp req resp (resp notFound) $
            choice "make-blocks" $ terminus methodPost "text/plain" $ jsonApp $
                \newGoals -> atomically $ modifyTVar' goalsRef (HashMap.unionWith (+) newGoals)
    notFound = Wai.responseLBS notFound404 [] "The on-demand worker's only endpoint is /make-block"
    setts = Warp.defaultSettings
        & Warp.setPort (fromIntegral port)
        & Warp.setHost host

worker :: TVar (HashMap ChainId Word) -> Logger -> Worker
worker goalsRef minerLogger _nonce _target cid work = do
    atomically $ do
        goals <- readTVar goalsRef
        guard (any (/= 0) goals)
        writeTVar goalsRef (HashMap.adjust (\g -> if g == 0 then 0 else pred g) cid goals)
    writeLog minerLogger L.Info $ "block demanded for chain " <> T.pack (show cid)
    return work
