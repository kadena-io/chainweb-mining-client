{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: WorkerUtils
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module WorkerUtils
( -- * Block Creation Time
  getCurrentTimeMicros
, injectTime
, encodeTimeToWord64

-- * Nonces
, injectNonce
, injectNonce_

-- * Check Target
, checkTarget
, fastCheckTarget
, powHash
, powHashToTargetWords
) where

import Crypto.Hash

import qualified Data.ByteArray as BA
import Data.Bytes.Signed
import qualified Data.ByteString.Short as BS
import Data.Int
import qualified Data.Memory.Endian as BA
import Data.Time.Clock.System
import Data.Word

import Foreign.Ptr (castPtr)
import Foreign.Storable (peekElemOff, pokeByteOff)

import GHC.Exts

-- internal modules

import Target

import Utils

import Worker

-- -------------------------------------------------------------------------- --
-- Block Creation Time

getCurrentTimeMicros :: IO Int64
getCurrentTimeMicros = do
    MkSystemTime secs nanos <- getSystemTime
    return $! secs * 1000000 + (int nanos `div` 1000)

injectTime :: Int64 -> Ptr Word8 -> IO ()
injectTime t buf = pokeByteOff buf 8 $ encodeTimeToWord64 t
{-# INLINE injectTime #-}

encodeTimeToWord64 :: Int64 -> Word64
encodeTimeToWord64 t = BA.unLE . BA.toLE $ unsigned t
{-# INLINE encodeTimeToWord64 #-}

-- -------------------------------------------------------------------------- --
-- Check Work Headers

-- | `injectNonce` makes low-level assumptions about the byte layout of a
-- hashed `BlockHeader`. If that layout changes, this functions need to be
-- updated. The assumption allows us to iterate on new nonces quickly.
--
-- Recall: `Nonce` contains a `Word64`, and is thus 8 bytes long.
--
-- See also: https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding
--
injectNonce_ :: Nonce -> Ptr Word8 -> IO ()
injectNonce_ (Nonce n) buf = pokeByteOff buf 278 $ le64 n
{-# INLINE injectNonce_ #-}

injectNonce :: Nonce -> Work -> IO Work
injectNonce n (Work bytes) = BS.useAsCStringLen bytes $ \(ptr, l) -> do
    injectNonce_ n (castPtr ptr)
    Work <$> BS.packCStringLen (ptr, l)
{-# INLINE injectNonce #-}

-- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers in
-- little endian encoding, hence we compare against the target from the end of
-- the bytes first, then move toward the front 8 bytes at a time.
--
fastCheckTarget :: TargetWords -> Ptr Word64 -> IO Bool
fastCheckTarget (TargetWords a b c d) !powPtr =
    checkTargetWordOff d 3 powPtr >>= \case
        LT -> return False
        GT -> return True
        EQ -> checkTargetWordOff c 2 powPtr >>= \case
            LT -> return False
            GT -> return True
            EQ -> checkTargetWordOff b 1 powPtr >>= \case
                LT -> return False
                GT -> return True
                EQ -> checkTargetWordOff a 0 powPtr >>= \case
                    LT -> return False
                    GT -> return True
                    EQ -> return True
{-# INLINE fastCheckTarget #-}

checkTargetWordOff :: Word64 -> Int -> Ptr Word64 -> IO Ordering
checkTargetWordOff !w !n !powPtr = compare w <$> peekElemOff powPtr n
{-# INLINE checkTargetWordOff #-}

checkTarget :: Target -> Work -> IO Bool
checkTarget t w = do
    t' <- powHashToTargetWords (powHash w)
    return $ targetFromWords t' <= t
{-# INLINE checkTarget #-}

powHashToTargetWords :: Digest Blake2s_256 -> IO TargetWords
powHashToTargetWords h = BA.withByteArray h $ \ptr -> TargetWords
    <$> peekWord64OffLe ptr 0
    <*> peekWord64OffLe ptr 8
    <*> peekWord64OffLe ptr 16
    <*> peekWord64OffLe ptr 24
{-# INLINE powHashToTargetWords #-}

powHash :: Work -> Digest Blake2s_256
powHash (Work bytes) = hash (BS.fromShort bytes)
{-# INLINE powHash #-}

