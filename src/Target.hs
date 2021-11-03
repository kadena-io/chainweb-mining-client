{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: Target
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Target
(
-- * Difficulty Level
  Level
, level

-- * Target
, Target(..)
, target
, getTargetLevel
, increaseLevel
, reduceLevel
, avgTarget
, nullTarget
, targetToText16
, targetToText16Be
, encodeTarget
, decodeTarget
, targetCompLe
, targetClz
, targetSet
) where

import Control.Monad.ST

import qualified Data.Aeson as A
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BS
import Data.Coerce
import Data.Hashable
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Exts
import GHC.Generics
import GHC.ST

import Text.Read

-- internal modules

import Utils

-- -------------------------------------------------------------------------- --
-- Difficulty Level

newtype Level = Level Int
    deriving (Show)

level :: Show a => Integral a => a -> Level
level i
    | i < 0 || i > 255 = error $ "Invalid difficulty level. Expected [0,31]; got " <> show i
    | otherwise = Level (int i)
{-# INLINE level #-}

plusLevel :: Int -> Level -> Level
plusLevel i (Level l) = level (max 0 (min 255 (i + l)))
{-# INLINE plusLevel #-}

minusLevel :: Int -> Level -> Level
minusLevel i = plusLevel (-i)
{-# INLINE minusLevel #-}

avgLevel :: Level -> Level -> Level
avgLevel (Level a) (Level b) = Level $ (a + b) `div` 2
{-# INLINE avgLevel #-}

-- -------------------------------------------------------------------------- --
-- Hash Target

-- | Hash target. A little endian encoded 256 bit (unsigned) word.
--
-- Cf. https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding#work-header-binary-format
--
-- NOTE: In serveral places throughout the codebase it is assumed that this
-- has exactly 32 bytes!
--
-- NOTE: in Stratum this value is represented as big endian encoded hexadecimal
-- JSON string.
--
-- TODO: some operations might be faster if we would stored this in
-- big-endian.
--
newtype Target = Target { _targetBytes :: BS.ShortByteString }
    deriving (Eq, Generic)
    deriving newtype (Hashable)
    deriving (IsString, A.ToJSON, A.FromJSON) via (ReversedHexEncodedShortByteStringN 32)

-- Arithmetic operations are done, assuming little endian byte order
--
instance Ord Target where
    compare = targetCompLe

instance Show Target where
    show (Target b) = "Target " <> show (ReversedHexEncodedShortByteString b)

instance Read Target where
    readPrec = do
        Ident "Target" <- lexP
        b <- readPrec @(ReversedHexEncodedShortByteStringN 32)
        return (Target $ coerce b)

nullTarget :: Target
nullTarget = targetSet 256
{-# INLINE nullTarget #-}

target :: Level -> Target
target (Level i) = targetSet i
{-# INLINE target #-}

getTargetLevel :: Target -> Level
getTargetLevel t = Level $ targetClz t
{-# INLINE getTargetLevel #-}

reduceLevel :: Int -> Target -> Target
reduceLevel i = target . minusLevel i . getTargetLevel
{-# INLINE reduceLevel #-}

increaseLevel :: Int -> Target -> Target
increaseLevel i = target . plusLevel i . getTargetLevel
{-# INLINE increaseLevel #-}

avgTarget :: Target -> Target -> Target
avgTarget a b = target $ avgLevel (getTargetLevel a) (getTargetLevel b)
{-# INLINE avgTarget #-}

decodeTarget :: MonadGet m => m Target
decodeTarget = Target . BS.toShort <$> getBytes 32
{-# INLINE decodeTarget #-}

encodeTarget :: MonadPut m => Target -> m ()
encodeTarget (Target b) = putByteString $ BS.fromShort b
{-# INLINE encodeTarget #-}

-- | Represent target bytes in hexadecimal base
--
targetToText16 :: Target -> T.Text
targetToText16 = shortByteStringToHex . _targetBytes
{-# INLINE targetToText16 #-}

-- | Represent target bytes in hexadecimal base in big endian encoding (used by Stratum)
--
targetToText16Be :: Target -> T.Text
targetToText16Be = T.decodeUtf8 . B16.encode . B.reverse . BS.fromShort . _targetBytes
{-# INLINE targetToText16Be #-}

-- -------------------------------------------------------------------------- --
-- Efficient Manipulation of Targets
--

-- All of the following assumes a 64bit system

-- | Create Target that has the given number of most significant
-- bits set to zero.
--
-- TODO: it may be faster to work with 4 Word64 values. It depends on
-- how fast setByteArray# is compared to writeWord64ByteArray#
--
targetSet :: Int -> Target
targetSet i@(I# z)
    | i < 1 && i > 256 = error "WorkerUtils.setTarget: illegal number of target bits."
    | otherwise = Target $ runST $ ST $ \s0 ->
        case newByteArray# 32# s0 of
            (# s1, b #) -> case set0 s1 b of
                s2 -> case set8 s2 b of
                    s3 -> case set1 s3 b of
                        s4 -> case unsafeFreezeByteArray# b s4 of
                            (# s5, a #) -> (# s5, BS.SBS a #)
  where
    -- Set clear all 256 bits
    set0 s b = setByteArray# b 0# 32# 0x00# s

    -- Set leading bc bytes
    set8 s b = setByteArray# b 0# c8 0xff# s

    -- Set leading bits of last byte
    set1 s b = if isTrue# (c8 <# 32#) then writeWord8Array# b c8 b1 s else s

    !o = 256# -# z
    !(# !c8, !c1 #) = quotRemInt# o 8#
    !b1 = shiftRL# 0xff## (8# -# c1)

-- | Count leading zeros of a target in little endian encoding.
--
-- It is an error if the input target does not contain exactly 32 bytes.
--
targetClz :: Target -> Int
targetClz (Target (BS.SBS x))
    | isTrue# (sizeofByteArray# x /=# 32#) = error "Worker.Utils.targetClz: target of invalid size"
    | otherwise = I# (go1 0#)
  where
    go1 a = let b = a +# word2Int# (clz64# (le64# (indexWord64Array# x 3#)))
        in if isTrue# (b <# 64#) then b else go2 b
    go2 a = let b = a +# word2Int# (clz64# (le64# (indexWord64Array# x 2#)))
        in if isTrue# (b <# 128#) then b else go3 b
    go3 a = let b = a +# word2Int# (clz64# (le64# (indexWord64Array# x 1#)))
        in if isTrue# (b <# 192#) then b else go4 b
    go4 a = a +# word2Int# (clz64# (le64# (indexWord64Array# x 0#)))

-- | Little endian comparision of two targets
--
-- It is an error if any of the input target does not contain exactly 32 bytes.
--
targetCompLe :: Target -> Target -> Ordering
targetCompLe (Target (BS.SBS b1)) (Target (BS.SBS b2))
    | isTrue# (sizeofByteArray# b1 /=# 32#) = error "Worker.Utils.targetCompLe: target of invalid size"
    | isTrue# (sizeofByteArray# b2 /=# 32#) = error "Worker.Utils.targetCompLe: target of invalid size"
    | otherwise = go1
  where
    go1 | isTrue# (ltWord# (le64# (indexWord64Array# b1 3#)) (le64# (indexWord64Array# b2 3#))) = LT
        | isTrue# (gtWord# (le64# (indexWord64Array# b1 3#)) (le64# (indexWord64Array# b2 3#))) = GT
        | otherwise = go2
    go2 | isTrue# (ltWord# (le64# (indexWord64Array# b1 2#)) (le64# (indexWord64Array# b2 2#))) = LT
        | isTrue# (gtWord# (le64# (indexWord64Array# b1 2#)) (le64# (indexWord64Array# b2 2#))) = GT
        | otherwise = go3
    go3 | isTrue# (ltWord# (le64# (indexWord64Array# b1 1#)) (le64# (indexWord64Array# b2 1#))) = LT
        | isTrue# (gtWord# (le64# (indexWord64Array# b1 1#)) (le64# (indexWord64Array# b2 1#))) = GT
        | otherwise = go4
    go4 | isTrue# (ltWord# (le64# (indexWord64Array# b1 0#)) (le64# (indexWord64Array# b2 0#))) = LT
        | isTrue# (gtWord# (le64# (indexWord64Array# b1 0#)) (le64# (indexWord64Array# b2 0#))) = GT
        | otherwise = EQ

