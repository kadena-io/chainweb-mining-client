{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
, fastCheckTarget
, fastCheckTarget_
) where

import Crypto.Hash

import qualified Data.ByteArray as BA
import Data.Bytes.Signed
import qualified Data.ByteString.Short as BS
import Data.Int
import qualified Data.Memory.Endian as BA
import Data.Time.Clock.System
import Data.Word

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, pokeByteOff)

-- internal modules

import Worker
import Utils

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
-- Targets and Nonces

-- | `injectNonce` makes low-level assumptions about the byte layout of a
-- hashed `BlockHeader`. If that layout changes, this functions need to be
-- updated. The assumption allows us to iterate on new nonces quickly.
--
-- Recall: `Nonce` contains a `Word64`, and is thus 8 bytes long.
--
-- See also: https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding
--
injectNonce_ :: Nonce -> Ptr Word8 -> IO ()
injectNonce_ (Nonce n) buf = pokeByteOff buf 278 n
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
fastCheckTarget_ :: Ptr Word64 -> Ptr Word64 -> IO Bool
fastCheckTarget_ !trgPtr !powPtr =
    fastCheckTargetN 3 trgPtr powPtr >>= \case
        LT -> return False
        GT -> return True
        EQ -> fastCheckTargetN 2 trgPtr powPtr >>= \case
            LT -> return False
            GT -> return True
            EQ -> fastCheckTargetN 1 trgPtr powPtr >>= \case
                LT -> return False
                GT -> return True
                EQ -> fastCheckTargetN 0 trgPtr powPtr >>= \case
                    LT -> return False
                    GT -> return True
                    EQ -> return True
{-# INLINE fastCheckTarget_ #-}

-- | Recall that `peekElemOff` acts like `drop` for the size of the type in
-- question. Here, this is `Word64`. Since our hash is treated as a `Word256`,
-- each @n@ knocks off a `Word64`'s worth of bytes, and there would be 4 such
-- sections (64 * 4 = 256).
--
-- This must never be called for @n >= 4@.
--
fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
fastCheckTargetN n trgPtr powPtr = compare
    <$> peekElemOff trgPtr n
    <*> peekElemOff powPtr n
{-# INLINE fastCheckTargetN #-}

fastCheckTarget :: Target -> Work -> IO Bool
fastCheckTarget (Target t) w =
    BA.withByteArray (powHash w) $ \ph ->
        BS.useAsCStringLen t $ \(pt, _) ->
            fastCheckTarget_ (castPtr pt) (castPtr ph)
{-# INLINE fastCheckTarget #-}

powHash :: Work -> Digest Blake2s_256
powHash (Work bytes) = hash (BS.fromShort bytes)
{-# INLINE powHash #-}
