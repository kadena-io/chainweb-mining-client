{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: JsonRpc
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module JsonRpc
(
-- * JSON Utils
  Static(..)

-- JSON RPC Message Ids
, MsgId(..)

-- JSON RPC Error
, Error(..)

-- * JSON RPC Request Messages
, requestProperties

-- * JSON RPC Result Messages
, responseProperties
, parseResponse
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.Types as A (Pair, Parser)
import qualified Data.Text as T

-- internal modules

-- -------------------------------------------------------------------------- --
-- JSON Utils

data Static (a :: k) = Static
    deriving (Show, Eq, Ord)

instance A.ToJSON (Static 'Nothing) where
    toEncoding _ = A.null_
    toJSON _ = A.Null
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance A.FromJSON (Static 'Nothing) where
    parseJSON A.Null = return Static
    parseJSON a = fail $ "expected \'null\' but got " <> show a
    {-# INLINE parseJSON #-}

instance A.ToJSON (Static 'True) where
    toEncoding _ = A.toEncoding True
    toJSON _ = A.toJSON True
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance A.FromJSON (Static 'True) where
    parseJSON = A.withBool "True" $ \b -> if b
        then return Static
        else fail "expected constant \'true\' but got \'false\'"
    {-# INLINE parseJSON #-}

instance A.ToJSON (Static 'False) where
    toEncoding _ = A.toEncoding False
    toJSON _ = A.toJSON False
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance A.FromJSON (Static 'False) where
    parseJSON = A.withBool "False" $ \b -> if b
        then fail "expected constant \'false\' but got \'true\'"
        else return Static
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- | JSON RPC Message Id
--
-- This is an integral number that is used to match result message to requests
-- within a JSON RPC session.
--
newtype MsgId = MsgId Int
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- -------------------------------------------------------------------------- --
-- JSON RPC Error

newtype Error = Error (Int, T.Text, A.Value)
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- -------------------------------------------------------------------------- --
-- | JSON RPC Request Messages
--
-- A remote method is invoked by sending a request to a remote service. The
-- request is a single object serialized using  JSON.
--
-- A notification is a special request which does not have a response. The
-- notification is a single object serialized using JSON.
--
-- * method - A String containing the name of the method to be invoked.
--
-- * params - An Array of objects to pass as arguments to the method.
--
-- * id - The request id. This can be of any type. It is used to match the
--   response with the request that it is replying to.
--
-- This is @null@ when the request if a notification. In that case no response
-- is expected.
--
-- In this implementation we limit the value to be an integral number of @null@.
--
requestProperties :: A.KeyValue kv => A.ToJSON a => T.Text -> a -> Maybe MsgId -> [kv]
requestProperties method params i =
    [ "method" A..= method
    , "params" A..= params
    , "id" A..= i
    ]
{-# INLINE requestProperties #-}
{-# SPECIALIZE requestProperties :: A.ToJSON a => T.Text -> a -> Maybe MsgId -> [A.Series] #-}
{-# SPECIALIZE requestProperties :: A.ToJSON a => T.Text -> a -> Maybe MsgId -> [A.Pair] #-}

-- -------------------------------------------------------------------------- --
-- | JSON RPC Result Messages
--
-- When the method invocation completes, the service must reply with a response.
-- The response is a single object serialized using  JSON.
--
-- It has three properties:
--
-- * result - The Object that was returned by the invoked method. This must be
--   null in case there was an error invoking the method.
--
-- It seems that for Stratum this actully is an array

-- * error - An Error object if there was an error invoking the method. It must be
--   null if there was no error.

-- * id - This must be the same id as the request it is responding to.
--
-- In this implementation we limit the value to be an integral number or @null@.
--
responseProperties
    :: A.KeyValue kv
    => A.ToJSON a
    => A.ToJSON b
    => MsgId
    -> Either a b
    -> [kv]
responseProperties i r =
    [ "result" A..= either (const Nothing) Just r
    , "error" A..= either Just (const Nothing) r
    , "id" A..= i
    ]
{-# INLINE responseProperties #-}
{-# SPECIALIZE responseProperties :: A.ToJSON a => A.ToJSON b => MsgId -> Either a b -> [A.Series] #-}
{-# SPECIALIZE responseProperties :: A.ToJSON a => A.ToJSON b => MsgId -> Either a b -> [A.Pair] #-}

parseResponse :: A.FromJSON a => A.FromJSON b => A.Object -> A.Parser (Either a b)
parseResponse o = o A..: "error" >>= \case
    Nothing -> o A..: "result"
    Just e -> return $ Left e
{-# INLINE parseResponse #-}

