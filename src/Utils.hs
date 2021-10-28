{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Utils
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Utils
(
-- * Hex encoded integral numbers
  HexInt(..)

-- * Hex-Encoded Short ByteString
, HexEncodedShortByteString(..)
, shortByteStringToHex
, shortByteStringFromHex

-- * Byte Swapped Hex-Encoded Short ByteStrings
, ReversedHexEncodedShortByteString(..)
, reversedShortByteStringToHex
, reversedShortByteStringFromHex

-- * Misc
, int
) where

import Data.Aeson
import Data.Aeson.Encoding hiding (int)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read as T

-- -------------------------------------------------------------------------- --
-- Hex Encoded Int

newtype HexInt a = HexInt { _getHexInt :: a}
    deriving newtype (Show, Read, Eq, Ord, Integral, Enum, Real, Num)

instance Integral a => ToJSON (HexInt a) where
    toEncoding = unsafeToEncoding . BB.wordHex . fromIntegral
    toJSON = toJSON . TB.toLazyText . TB.hexadecimal
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance (Show a, Integral a) => FromJSON (HexInt a) where
    parseJSON = withText "HexInt" $ \t -> case T.hexadecimal t of
        Right (n, "") -> return n
        Right (n, x) -> fail $ "failed to parse hex encoded integral number: pending characters after reading " <> show n <> ": " <> T.unpack x
        Left e -> fail $ "failed to read hex encoded integral number: " <> e

-- -------------------------------------------------------------------------- --
-- Hex Encoded Short ByteString

newtype HexEncodedShortByteString = HexEncodedShortByteString
    { _getHexEncodedShortByteString :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

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
-- ByteSwapped Hex Encoding

newtype ReversedHexEncodedShortByteString = ReversedHexEncodedShortByteString
    { _getReversedHexEncodedShortByteString :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

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
-- Misc

int :: Integral a => Num b => a -> b
int = fromIntegral

