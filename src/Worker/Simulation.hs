{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Worker.Simulation
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Simulation Mining Worker
--
-- A fake mining worker that is not actually doing any work. It calculates the
-- solve time base on the assumed hash power of the worker thread and returns
-- the work bytes unchanged after that time has passed.
--
module Worker.Simulation
( HashRate(..)
, defaultHashRate
, simulationWorker
) where

import Control.Concurrent (threadDelay)

import Data.Aeson
import qualified Data.ByteString.Short as BS
import Data.Hashable
import qualified Data.Text as T

import GHC.Generics

import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Logger
import Worker

-- -------------------------------------------------------------------------- --
-- HashRate

newtype HashRate = HashRate Double
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Read, Num, Fractional, Floating, Real, Enum, Hashable, ToJSON, FromJSON)

-- | Default is 1MH
defaultHashRate :: HashRate
defaultHashRate = 1_000_000

-- -------------------------------------------------------------------------- --
-- Simulation Mining Worker

-- | A fake mining worker that is not actually doing any work. It calculates the
-- solve time base on the assumed hash power of the worker thread and returns
-- the work bytes unchanged after that time has passed.
--
simulationWorker :: Logger -> MWC.GenIO -> HashRate -> Worker
simulationWorker logger rng rate _nonce (Target targetBytes) work = do
    delay <- round <$> MWC.exponential scale rng
    logg Info $ "solve time (microseconds): " <> T.pack (show delay)
    threadDelay delay
    return work
  where
    logg = writeLog logger

    -- expectedMicros = 1_000_000 * difficulty / rate

    -- MWC.exponential is parameterized by the rate, i.e. 1 / expected_time
    scale = realToFrac $ realToFrac rate / (difficulty * 1_000_000)

    -- the expected number of attempts for solving a target is the difficulty.
    difficulty :: Rational
    difficulty = 2^(256 :: Integer) / targetNum

    -- Target is an little endian encoded (unsigned) 256 bit word.
    targetNum :: Rational
    targetNum = foldr (\b a -> fromIntegral b + 256 * a) 0 $ BS.unpack targetBytes

