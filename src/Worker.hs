{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Worker
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The type of a mining worker
--
module Worker
(
-- * Hash Target
  Target(..)
, encodeTarget
, decodeTarget
, targetToText16
, targetToText16Be

-- * Mining Work
, Work(..)
, encodeWork
, decodeWork

-- * Nonce
, Nonce(..)

-- * Mining Worker
, Worker
) where

import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as BS
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

import GHC.Generics

-- internal modules

import Utils

-- -------------------------------------------------------------------------- --
-- Hash Target

-- | Hash target. A little endian encoded 256 bit (unsigned) word.
--
-- Cf. https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding#work-header-binary-format
--
newtype Target = Target { _targetBytes :: BS.ShortByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

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
-- Work

-- | Work bytes. The last 8 bytes are the nonce that is updated by the miner
-- while solving the work.
--
-- Cf. https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding#work-header-binary-format
--
newtype Work = Work BS.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

decodeWork :: MonadGet m => m Work
decodeWork = Work . BS.toShort <$> getBytes 286
{-# INLINE decodeWork #-}

encodeWork :: MonadPut m => Work -> m ()
encodeWork (Work b) = putByteString $ BS.fromShort b
{-# INLINE encodeWork #-}

-- -------------------------------------------------------------------------- --
-- Nonce

-- | POW Nonce
--
newtype Nonce = Nonce Word64
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

-- -------------------------------------------------------------------------- --
-- Mining Worker

type Worker = Nonce -> Target -> Work -> IO Work

