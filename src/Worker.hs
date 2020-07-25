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
import qualified Data.ByteString.Short as BS
import Data.Hashable
import Data.Word

import GHC.Generics

-- -------------------------------------------------------------------------- --
-- Hash Target

-- | Hash target. A little endian encoded 256 bit (unsigned) word.
--
-- Cf. https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding#work-header-binary-format
--
newtype Target = Target BS.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

decodeTarget :: MonadGet m => m Target
decodeTarget = Target . BS.toShort <$> getBytes 32

encodeTarget :: MonadPut m => Target -> m ()
encodeTarget (Target b) = putByteString $ BS.fromShort b

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

encodeWork :: MonadPut m => Work -> m ()
encodeWork (Work b) = putByteString $ BS.fromShort b

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

