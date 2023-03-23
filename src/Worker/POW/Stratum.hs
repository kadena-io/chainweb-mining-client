{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Worker.POW.Stratum
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Kadena Stratum Mining Protocol
--
-- cf. https://gist.github.com/mightybyte/f1567c2bec0380539c638225fb8c1cf4
--
-- TODO
--
-- * Create datatype for Error codes
-- * Find out if there is some standard for reporting errors
-- * proper reporting of discarded and stale shares (what are the precise modes)
--
-- Open Questions
-- * is authorizaiton required or optional (what are possible results)
-- * meaning of result of submit
-- * precise meaning notify 'clear' field
--
module Worker.POW.Stratum
( submitWork
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Catch

import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T

import qualified System.LogLevel as L

-- internal modules

import Logger
import Target
import Utils
import Worker
import WorkerUtils
import Worker.POW.Stratum.Protocol
import Worker.POW.Stratum.Server

-- -------------------------------------------------------------------------- --
-- Worker Interface

-- | This is called from the mining loop of chainweb-mining-client:
--
-- It is recommended to start several worker threads, so that there are always
-- enough active work items available.
--
submitWork :: StratumServerCtx -> Logger -> Nonce -> Target -> ChainId -> Work -> IO Work
submitWork ctx l nonce trg _cid work = withLogTag l "Stratum Worker" $ \logger ->
    let run w = waitForFirst
            (runJob ctx logger nonce trg w)
            (threadDelay jobRateMicros >> run (incrementTimeMicros jobRateMicros w))
    in run work
  where
    jobRateMicros :: Integral a => a
    jobRateMicros = 1000 * fromIntegral (_ctxRate ctx)

    waitForFirst :: IO Work -> IO Work -> IO Work
    waitForFirst a b = race a b >>= \case
        Right x -> return x
        Left x -> return x


runJob :: StratumServerCtx -> Logger -> Nonce -> Target -> Work -> IO Work
runJob ctx logger _nonce trg work = mask $ \umask -> do
    job <- umask $ newJob logger ctx trg work
    flip onException (writeLog logger L.Info ("discarded unfinished job: "  <> sshow (_jobId job))) $
        flip finally (removeJob ctx (_jobId job)) $ umask $ checkJob job
  where
    -- Check that the solution for a job is correct. This should never fail.
    -- Sessions should only submit shares that are actually solving the
    -- block.
    checkJob job = do
        nonce <- takeMVar (_jobResult job) -- at this point the mvar is available again
        let !w = injectNonce nonce (_jobWork job)
        checkTarget (_jobTarget job) w >>= \case
            True -> do
                writeLog logger L.Info $ "submitted job " <> sshow (_jobId job)
                return w
            False -> do
                writeLog logger L.Error $ "rejected job: invalid result " <> sshow (_jobId job)
                writeLog logger L.Info $ "invalid nonce: " <> sshow nonce
                    <> ", target: " <> sshow (_jobTarget job)
                    <> ", job work: " <> sshow (_jobWork job)
                    <> ", result work: " <> sshow w
                    <> ". Continue with job"
                checkJob job

newJob :: Logger -> StratumServerCtx -> Target -> Work -> IO Job
newJob logger ctx trg work = do

    -- Create new job
    jid <- atomicModifyIORef' (_ctxCurrentId ctx) (\x -> (nextJobId x, x))
    job <- Job jid trg work <$> newEmptyMVar

    flip onException (removeJob ctx jid) $ do

        -- add job to the job table
        atomically $ modifyTVar' (_ctxJobs ctx) $ HM.insert jid job

        -- notify all active connections
        -- (no need to do this in the same tx as above)
        atomically $ writeTVar (_ctxCurrentJob ctx) job

        writeLog logger L.Info $ "created new job " <> T.pack (show (_jobId job))
        return job

removeJob :: StratumServerCtx -> JobId -> IO ()
removeJob ctx jid = atomically $ modifyTVar' (_ctxJobs ctx) $ HM.delete jid

