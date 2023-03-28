{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Worker.POW.CPU
-- Copyright: Copyright © 2020-2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Worker.POW.CPU
( cpuWorker
, authenticatedCpuWorker
) where

import Control.Concurrent

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
--

-- | Single threaded CPU mining worker
--

cpuWorker
  :: forall a
  . HashAlgorithm a
  => Logger
  -> Worker
cpuWorker = cpuWorkerInternal @a cpuMiningLoop

cpuMiningLoop :: MiningLoop
cpuMiningLoop buf initialNonce checkNonce = do
    -- inner mining loop
    --
    let go1 0 n = return (Just n)
        go1 !i n@(Nonce nv) = do
            checkNonce n >>= \case
                True -> return Nothing
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
    go0 100000 t initialNonce

-- -------------------------------------------------------------------------- --
-- Create Authenticated Blocks

-- | Single threaded CPU mining worker for authenticated blocks
--
-- This is an inefficient CPU worker that authenticates nonces with a key. It
-- allows to test decentralized PoW in public network while limiting block
-- creation to trusted miners that share the block authentication secret key.
--
-- Difficulty is very low, because generation of blocks depend on clock ticks
-- which happen at microsecond granularity. The hash rate is thus limited at
-- 1MHZ (i.e. 1 MH/s).
--
authenticatedCpuWorker
    :: forall a
    . HashAlgorithm a
    => BlockAuthenticationKey
    -> Logger
    -> Worker
authenticatedCpuWorker key = cpuWorkerInternal @a (authenticedBlockMiningLoop key)

authenticedBlockMiningLoop :: BlockAuthenticationKey -> MiningLoop
authenticedBlockMiningLoop key buf _ checkNonce = go
  where
    go = do
        t <- getCurrentTimeMicros
        injectTime_ t buf
        checkNonce (authenticatedNonce_ key buf) >>= \case
            True -> return ()
            False -> yield >> go
                -- we'd rather block by something like 500ns, but
                -- that's not supported by threadDelay.
                -- alternatively we could spin without computing the hash,
                -- but this worker is only for testing anyways.

-- -------------------------------------------------------------------------- --
-- Internal

type MiningLoop = Ptr Word8 -> Nonce -> (Nonce -> IO Bool) -> IO ()

cpuWorkerInternal
    :: forall a
    . HashAlgorithm a
    => MiningLoop
    -> Logger
    -> Worker
cpuWorkerInternal miningLoop logger initialNonce trg _cid work = do
    attemptsRef <- newIORef @Int 0
    !ctx <- hashMutableInit @a
    new <- BA.copy hbytes $ \buf ->
        allocaBytes (powSize :: Int) $ \pow ->
            miningLoop buf initialNonce $ \nonce -> do

                -- check Nonce
                injectNonce_ nonce buf
                hashMutableReset ctx
                BA.withByteArray ctx $ \ctxPtr -> do
                    hashInternalUpdate @a ctxPtr buf $ fromIntegral bufSize
                    hashInternalFinalize ctxPtr $ castPtr pow
                modifyIORef' attemptsRef (+ 1)
                fastCheckTarget trgWords (castPtr pow)

    attempts <- readIORef attemptsRef
    writeLog logger Info $ "Solved header with " <> T.pack (show attempts) <> " attempts."
    return (Work $ BS.toShort new)
  where
    !trgWords = targetToWords trg
    !hbytes = let (Work b) = work in BS.fromShort b

    bufSize :: Int
    !bufSize = B.length hbytes

    powSize :: Int
    !powSize = hashDigestSize @a undefined

