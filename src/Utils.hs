{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Utils
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Utils
(
-- Hexadecimal Encoding for Integral Types
  IntHexText(..)

-- * Hex-Encoded Short ByteString
, HexEncodedShortByteString(..)
, shortByteStringToHex
, shortByteStringFromHex

-- * Byte Swapped Hex-Encoded Short ByteStrings
, ReversedHexEncodedShortByteString(..)
, reversedShortByteStringToHex
, reversedShortByteStringFromHex

-- * Byte Swapped Hex-Encoded Short ByteStrings of Static Length
, ReversedHexEncodedShortByteStringN(..)

-- * Misc
, nat
, int
, sshow
, quoted
, le64
, le64#
, seconds
, secondsNs
, writeTMVar
) where

import Control.Concurrent.STM

import Data.Aeson
import Data.Aeson.Encoding hiding (int)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Function
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read as T
import Data.Word

import GHC.ByteOrder
import GHC.Exts
import GHC.TypeNats

import System.Clock

import Text.Read

-- -------------------------------------------------------------------------- --
--
-- | Hexadecimal Encoding for Integral Types
--
newtype IntHexText a = IntHexText { _getIntHexText :: a}
    deriving newtype (Eq, Ord, Integral, Enum, Real, Num)

instance (Integral a) => Show (IntHexText a) where
    show = TL.unpack . TB.toLazyText . TB.hexadecimal
    {-# INLINE show #-}

instance (Integral a) => ToJSON (IntHexText a) where
    toEncoding = unsafeToEncoding . quoted . BB.wordHex . int
    toJSON = toJSON . TB.toLazyText . TB.hexadecimal
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance (Show a, Integral a) => FromJSON (IntHexText a) where
    parseJSON = withText "IntHexText" $ \t -> case T.hexadecimal t of
        Right (n, "") -> return n
        Right (n, x) -> fail $ "failed to parse hex encoded integral number: pending characters after reading " <> show n <> ": " <> T.unpack x
        Left e -> fail $ "failed to read hex encoded integral number: " <> e
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
--
-- | Hex Encoded Short ByteString
--
newtype HexEncodedShortByteString = HexEncodedShortByteString
    { _getHexEncodedShortByteString :: BS.ShortByteString }
    deriving (Eq, Ord)

instance IsString HexEncodedShortByteString where
    fromString a = case shortByteStringFromHex $ T.pack a of
        Right x -> HexEncodedShortByteString x
        Left e -> error $ "failed to parse hex encoded byte string: " <> e

instance Show HexEncodedShortByteString where
    show = show . T.unpack . shortByteStringToHex . _getHexEncodedShortByteString

instance Read HexEncodedShortByteString where
    readPrec = do
        str <- readPrec
        case shortByteStringFromHex (T.pack str) of
            Right x -> return $ HexEncodedShortByteString x
            Left err -> fail err

instance ToJSON HexEncodedShortByteString where
    toEncoding = toEncoding . shortByteStringToHex . _getHexEncodedShortByteString
    toJSON = toJSON . shortByteStringToHex . _getHexEncodedShortByteString
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON HexEncodedShortByteString where
    parseJSON = withText "HexEncodedBytes" $ \t ->
        case shortByteStringFromHex t of
            Right x -> return $ HexEncodedShortByteString x
            Left err -> fail err
    {-# INLINE parseJSON #-}

-- | Represent a 'BS.ShortByteString' in hexadecimal base
--
shortByteStringToHex :: BS.ShortByteString -> T.Text
shortByteStringToHex = T.decodeUtf8 . B16.encode . BS.fromShort
{-# INLINE shortByteStringToHex #-}

shortByteStringFromHex :: T.Text -> Either String BS.ShortByteString
shortByteStringFromHex = fmap BS.toShort . B16.decode . T.encodeUtf8
{-# INLINE shortByteStringFromHex #-}

-- -------------------------------------------------------------------------- --
--
-- | ByteSwapped Hex Encoding
-- Comparision is done lexicographically starting with the most significant bytes.
--
newtype ReversedHexEncodedShortByteString = ReversedHexEncodedShortByteString
    { _getReversedHexEncodedShortByteString :: BS.ShortByteString }
    deriving (Eq)

instance Ord ReversedHexEncodedShortByteString where
    compare = compare `on` (B.reverse . BS.fromShort . _getReversedHexEncodedShortByteString)

instance IsString ReversedHexEncodedShortByteString where
    fromString a = case reversedShortByteStringFromHex $ T.pack a of
        Right x -> ReversedHexEncodedShortByteString x
        Left e -> error $ "failed to parse hex encoded byte string: " <> e

instance Show ReversedHexEncodedShortByteString where
    show = show . T.unpack . reversedShortByteStringToHex . _getReversedHexEncodedShortByteString

instance Read ReversedHexEncodedShortByteString where
    readPrec = do
        str <- readPrec @T.Text
        case reversedShortByteStringFromHex str of
            Right x -> return $ ReversedHexEncodedShortByteString x
            Left err -> fail err

instance ToJSON ReversedHexEncodedShortByteString where
    toEncoding = toEncoding . reversedShortByteStringToHex . _getReversedHexEncodedShortByteString
    toJSON = toJSON . reversedShortByteStringToHex . _getReversedHexEncodedShortByteString
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON ReversedHexEncodedShortByteString where
    parseJSON = withText "ReversedHexEncodedBytes" $ \t ->
        case reversedShortByteStringFromHex t of
            Right x -> return $ ReversedHexEncodedShortByteString x
            Left err -> fail err
    {-# INLINE parseJSON #-}

-- | Represent a 'BS.ShortByteString' in hexadecimal base
--
reversedShortByteStringToHex :: BS.ShortByteString -> T.Text
reversedShortByteStringToHex = T.decodeUtf8 . B16.encode . B.reverse . BS.fromShort
{-# INLINE reversedShortByteStringToHex #-}

reversedShortByteStringFromHex :: T.Text -> Either String BS.ShortByteString
reversedShortByteStringFromHex = fmap (BS.toShort . B.reverse) . B16.decode . T.encodeUtf8
{-# INLINE reversedShortByteStringFromHex #-}

-- -------------------------------------------------------------------------- --
--
-- | ByteSwapped Hex Encoding Of Static Length
--
-- Comparision is done lexicographically starting with the most significant bytes.
--
newtype ReversedHexEncodedShortByteStringN (n :: Nat) = ReversedHexEncodedShortByteStringN
    { _getReversedHexEncodedShortByteStringN :: ReversedHexEncodedShortByteString }
    deriving newtype (Eq, Ord, Show, ToJSON)

reversedHexEncodedShortByteStringN
    :: forall n
    . KnownNat n
    => ReversedHexEncodedShortByteString
    -> Either T.Text (ReversedHexEncodedShortByteStringN n)
reversedHexEncodedShortByteStringN r@(ReversedHexEncodedShortByteString a)
    | BS.length a /= nat @n = Left $ "ReversedHexEncodedShortByteStringN has wrong length. Expected " <> sshow @Int (nat @n) <> " got " <> sshow (BS.length a)
    | otherwise = Right $ ReversedHexEncodedShortByteStringN r

instance KnownNat n => IsString (ReversedHexEncodedShortByteStringN n) where
    fromString = either (error . T.unpack) id
        . reversedHexEncodedShortByteStringN
        . fromString

instance KnownNat n => Read (ReversedHexEncodedShortByteStringN n) where
    readPrec = do
        a <- reversedHexEncodedShortByteStringN @n <$> readPrec
        either (fail . T.unpack) return a

instance KnownNat n => FromJSON (ReversedHexEncodedShortByteStringN n) where
    parseJSON v = do
        a <- reversedHexEncodedShortByteStringN @n <$> parseJSON v
        either (fail . T.unpack) return a
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Misc

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

sshow :: Show a => IsString s => a -> s
sshow = fromString . show
{-# INLINE sshow #-}

quoted :: IsString a => Monoid a => a -> a
quoted b = "\"" <> b <> "\""
{-# INLINE quoted #-}

nat :: forall (n :: Nat) a . KnownNat n => Integral a => a
nat = int $ natVal' (proxy# :: Proxy# n)

-- | Encode to or from little endian. This is @id@ on little endian platforms.
--
le64 :: Word64 -> Word64
le64 = f targetByteOrder
  where
    f BigEndian = byteSwap64
    f LittleEndian = id
    {-# INLINE f #-}
{-# INLINE le64 #-}

-- | Encode to or from little endian. This is @id@ on little endian platforms.
--
le64# :: Word# -> Word#
le64# = f targetByteOrder
  where
    f BigEndian x = byteSwap64# x
    f LittleEndian x = x
    {-# INLINE f #-}
{-# INLINE le64# #-}

seconds :: Integer -> TimeSpec
seconds i = fromNanoSecs $ i * 1_000_000_000
{-# INLINE seconds #-}

secondsNs :: Integer -> Integer
secondsNs i = i * 1_000_000_000
{-# INLINE secondsNs #-}

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a = tryTakeTMVar var >> putTMVar var a
{-# INLINE writeTMVar #-}

