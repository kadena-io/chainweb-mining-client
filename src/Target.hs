{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
-- * Target
  Target(..)
, mkTarget
, nullTarget
, avgTarget

-- * Target Words
, TargetWords(..)
, targetToWords
, targetFromWords

-- * Binary Encodings
, encodeTarget
, decodeTarget

-- * Textual Encodings
, targetToText16Le
, targetToText16Be

-- * Difficulty Level
, Level
, level
, mkTargetLevel
, getTargetLevel
, increaseLevel
, reduceLevel
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Hashable
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Word

import GHC.Generics
import Numeric.Natural

import Text.Read

-- internal modules

import Utils

-- -------------------------------------------------------------------------- --
-- Target

-- | Hash target. A little endian encoded 256 bit (unsigned) word.
--
-- Cf. https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding#work-header-binary-format
--
-- This is encoded as a positive integer with that it is smaller than 2^256
--
-- Binary encoding in a block header is fixed size 256 little endian format
-- (padded with zero bits).
--
-- Textual endcoding in JSON and debugging and log messages is fixed size 64
-- hexadecimal (padded with zeros).
--
-- All arithmetic is done is infinite precision rational arithmetic with
-- the final result capped at 2^256-1.
--
newtype Target = Target Natural
    deriving (Generic)
    deriving newtype (Eq, Ord, Enum, Hashable)

instance Bounded Target where
    minBound = Target 0
    maxBound = Target (2^256-1)

mkTarget :: MonadFail m => Integral a => a -> m Target
mkTarget a
    | a < 0 = fail "newTarget: target can not be smaller than zero"
    | a >= 2^(256 :: Int) = fail "newTarget: target can not be larger than 2^256-1"
    | otherwise = return $ Target $ int a

nullTarget :: Target
nullTarget = minBound

maxTarget :: Target
maxTarget = maxBound

avgTarget :: Target -> Target -> Target
avgTarget (Target a) (Target b) = Target $ (a + b) `div` 2
{-# INLINE avgTarget #-}

-- -------------------------------------------------------------------------- --
-- Representation as Word

-- Little Endian ordered. The words themself are in host byte ordering.
--
data TargetWords = TargetWords
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64

-- TODO on 64 bit platform we could use mkNatural instead
--
targetFromWords :: TargetWords -> Target
targetFromWords (TargetWords a b c d) = Target $
    ((int d * 0x10000000000000000 + int c) * 0x10000000000000000 + int b) * 0x10000000000000000 + int a
{-# INLINE targetFromWords #-}

-- TODO with GHC-9 and ghc-bignum we could switch the type of Target to
-- use BigNat on 64 bit systems.
--
targetToWords :: Target -> TargetWords
targetToWords (Target i0) = TargetWords (int a) (int b) (int c) (int d)
  where
    (i1, a) = i0 `quotRem` 0x10000000000000000
    (i2, b) = i1 `quotRem` 0x10000000000000000
    (d, c) = i2 `quotRem` 0x10000000000000000
{-# INLINE targetToWords #-}

-- -------------------------------------------------------------------------- --
-- Binary Encodings

decodeTarget :: MonadGet m => m Target
decodeTarget = fmap targetFromWords $
    TargetWords
        <$> getWord64le
        <*> getWord64le
        <*> getWord64le
        <*> getWord64le
{-# INLINE decodeTarget #-}

encodeTarget :: MonadPut m => Target -> m ()
encodeTarget t = do
    putWord64le a
    putWord64le b
    putWord64le c
    putWord64le d
  where
    TargetWords a b c d = targetToWords t
{-# INLINE encodeTarget #-}

-- -------------------------------------------------------------------------- --
-- Textual Encodings

targetBuilderHexBe :: Target -> BB.Builder
targetBuilderHexBe t
    = BB.word64HexFixed d
    <> BB.word64HexFixed c
    <> BB.word64HexFixed b
    <> BB.word64HexFixed a
  where
    TargetWords a b c d = targetToWords t
{-# INLINE targetBuilderHexBe #-}

targetBuilderHexLe :: Target -> BB.Builder
targetBuilderHexLe t
    = BB.word64HexFixed a
    <> BB.word64HexFixed b
    <> BB.word64HexFixed c
    <> BB.word64HexFixed d
  where
    TargetWords a b c d = targetToWords t
{-# INLINE targetBuilderHexLe #-}

instance Show Target where
    show t = "Target " <> show (targetToText16Be t)
    {-# INLINE show #-}

instance Read Target where
    readPrec = do
        Ident "Target" <- lexP
        readPrec >>= targetFromText16Be

instance IsString Target where
    fromString s = case targetFromText16Be (T.pack s) of
        Just x -> x
        Nothing -> error "Target.IsString.fromString: failed to parse target"
    {-# INLINE fromString #-}

instance A.ToJSON Target where
    toEncoding = A.unsafeToEncoding . quoted . targetBuilderHexBe
    toJSON = A.toJSON . targetToText16Be
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance A.FromJSON Target where
    parseJSON = A.withText "Target" targetFromText16Be
    {-# INLINE parseJSON #-}

targetFromText16Be :: MonadFail m => T.Text -> m Target
targetFromText16Be t
    | T.length t /= 64 =
        fail $ "Target string has wrong length. Expected 64 ," <> " got " <> sshow (T.length t)
    | otherwise = case T.hexadecimal t of
        Right (n, "") -> mkTarget @_ @Natural n
        Right (n, x) -> fail $ "failed to parse target: pending characters after reading " <> show n <> ": " <> T.unpack x
        Left e -> fail $ "failed to parse target: " <> e
{-# INLINE targetFromText16Be #-}

-- | Represent target bytes in hexadecimal base in little endian byte order
--
targetToText16Le :: Target -> T.Text
targetToText16Le = T.decodeUtf8 . LB.toStrict . BB.toLazyByteString . targetBuilderHexLe
{-# INLINE targetToText16Le #-}

-- | Represent target bytes in hexadecimal base in big endian byte order (used by Stratum)
--
targetToText16Be :: Target -> T.Text
targetToText16Be = T.decodeUtf8 . LB.toStrict . BB.toLazyByteString . targetBuilderHexBe
{-# INLINE targetToText16Be #-}

-- -------------------------------------------------------------------------- --
-- Difficulty

-- TODO: is double fine here, or should we use Ratio?
--
newtype Difficulty = Difficulty Double
    deriving (Show)
    deriving newtype (Eq, Ord)

-- targetDifficulty :: Target -> Difficulty
-- targetDifficulty (Target t) = m / t
--   where
--     Target m = maxTarget


-- -------------------------------------------------------------------------- --
-- Difficulty Levels (leading zeros)

newtype Level = Level Int
    deriving (Show, Eq, Ord)

level :: Show a => Integral a => a -> Level
level i
    | i < 0 || i > 255 = error $ "Invalid difficulty level. Expected [0,31]; got " <> show i
    | otherwise = Level (int i)
{-# INLINE level #-}

mkTargetLevel :: Level -> Target
mkTargetLevel (Level i) = Target $ 2^(256-i) - 1

getTargetLevel :: Target -> Level
getTargetLevel (Target i) = Level $ int $ 256 - naturalLog2 (2 * i)
{-# INLINE getTargetLevel #-}

reduceLevel :: Int -> Target -> Target
reduceLevel i (Target t) = Target $ shiftL t i
{-# INLINE reduceLevel #-}

increaseLevel :: Int -> Target -> Target
increaseLevel i (Target t) = Target $ shiftR t i
{-# INLINE increaseLevel #-}

