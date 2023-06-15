{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Worker.ConstantDelay
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- Simulation Mining Worker
--
-- A mining worker that is not actually doing any work.
-- It returns the work bytes unchanged after a constant delay has passed.
--
module Worker.ConstantDelay (constantDelayWorker) where

import Control.Concurrent (threadDelay)

import qualified Data.Text as T

import Numeric.Natural

import System.LogLevel

-- internal modules

import Logger
import Worker

-- -------------------------------------------------------------------------- --
-- Simulation Mining Worker

-- | A mining worker that is not actually doing any work. It returns the work
-- bytes unchanged after a configured time delay passes.
--
constantDelayWorker :: Logger -> Natural -> Worker
constantDelayWorker logger delay _nonce _target _cid work = do
    logg Info $ "solve time (seconds): " <> T.pack (show delay)
    threadDelay ((1_000000 * fromIntegral delay) `div` 3)
    return work
  where
    logg = writeLog logger

