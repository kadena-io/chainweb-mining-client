{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Worker.Stratum.Protocol
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Worker.Stratum.Protocol
(
-- * Nonces

-- ** Nonce Size
  NonceSize
, nonceSize
, nonceSize'
, complementNonceSize

-- ** Nonce1
, Nonce1
, nonce1
, nonce1Size
, parseNonce1
, deriveNonce1

-- ** Nonce2
, Nonce2
, nonce2
, nonce2Size
, parseNonce2

, composeNonce

-- * Messages

-- ** Misc Types
, Agent
, Username
, Password
, JobId
, noJobId
, nextJobId
, ClientWorker

-- ** Requests
, MiningRequest(..)
, parseMiningRequest

-- ** Responses
, MiningResponse(..)
, parseMiningResponse
, subscribeResponse
, authorizeResponse
, authorizeError

-- ** Notificates
, MiningNotification(..)
) where

import Control.Applicative
import Control.Monad

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Bifunctor
import Data.Bits
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T

-- internal modules

import JsonRpc
import Target
import Utils
import Worker

-- -------------------------------------------------------------------------- --
-- Nonce Sizes

-- | Size of a nonce in bytes. This is a number between 0 and 8.
--
newtype NonceSize = NonceSize Int
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- | Smart Constructor for NonceSize
--
nonceSize :: Integral a => a -> Maybe NonceSize
nonceSize (int -> a)
    | a >= 0 && a <= 8 = Just $ NonceSize a
    | otherwise = Nothing
{-# INLINE nonceSize #-}

nonceSize' :: Show a => Integral a => a -> NonceSize
nonceSize' a = fromMaybe (error $ "Invalid Nonce Size: " <> sshow a) $ nonceSize a
{-# INLINE nonceSize' #-}

complementNonceSize :: NonceSize -> NonceSize
complementNonceSize (NonceSize a) = NonceSize (8 - a)
{-# INLINE complementNonceSize #-}

-- -------------------------------------------------------------------------- --
-- Nonce1

-- | The most significant bytes of the nonce. These are set by the pool.
--
-- The nonce bytes are injected into the work header in little endian encoding:
-- @Nonce2 <> Nonce1@
--
-- In stratum messages nonce bytes are encoded as hex strings with big endian
-- byte order.
--
data Nonce1 = Nonce1 NonceSize Nonce
    deriving (Eq, Ord)

-- | Smart Constructor for Nonce1
--
nonce1 :: NonceSize -> Nonce -> Maybe Nonce1
nonce1 (NonceSize s) n@(Nonce w)
    | w < 2 ^ (s * 8) = Just $ Nonce1 (NonceSize s) n
    | otherwise = Nothing
{-# INLINE nonce1 #-}

nonce1Size :: Nonce1 -> NonceSize
nonce1Size (Nonce1 s _) = s
{-# INLINE nonce1Size #-}

instance Show Nonce1 where
    show (Nonce1 _ n) = show n

instance A.ToJSON Nonce1 where
    toEncoding (Nonce1 _ n) = A.toEncoding n
    toJSON (Nonce1 _ n) = A.toJSON n
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

parseNonce1 :: NonceSize -> A.Value -> A.Parser Nonce1
parseNonce1 s v = do
    n <- A.parseJSON v
    case nonce1 s n of
        Nothing -> fail $ "Nonce with invalid size (expected size " <> show s <> "): " <> show n
        Just x -> return x
{-# INLINE parseNonce1 #-}

deriveNonce1 :: NonceSize -> Int -> String -> Nonce1
deriveNonce1 ns@(NonceSize s) salt input = Nonce1 ns
    $ Nonce (shiftR (int $ hashWithSalt salt input) (8 * (8 - int s)))

-- -------------------------------------------------------------------------- --
-- Nonce2

-- | The least significant bytes of the nonce. These respresent the share of
-- work performed by the client.
--
-- The nonce bytes are injected into the work header in little endian encoding:
-- @Nonce2 <> Nonce1@
--
-- In stratum messages nonce bytes are encoded as hex strings with big endian
-- byte order.
--
data Nonce2 = Nonce2 NonceSize Nonce
    deriving (Eq, Ord)

-- | Smart Constructor for Nonce2
--
nonce2 :: NonceSize -> Nonce -> Maybe Nonce2
nonce2 (NonceSize s) n@(Nonce w)
    | w < 2 ^ (s * 8) = Just $ Nonce2 (NonceSize s) n
    | otherwise = Nothing
{-# INLINE nonce2 #-}

nonce2Size :: Nonce2 -> NonceSize
nonce2Size (Nonce2 s _) = s
{-# INLINE nonce2Size #-}

instance Show Nonce2 where
    show (Nonce2 _ n) = show n

instance A.ToJSON Nonce2 where
    toEncoding (Nonce2 _ n) = A.toEncoding n
    toJSON (Nonce2 _ n) = A.toJSON n
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

parseNonce2 :: NonceSize -> A.Value -> A.Parser Nonce2
parseNonce2 s v = do
    n <- A.parseJSON v
    case nonce2 s n of
        Nothing -> fail $ "Nonce with invalid size (expected size " <> show s <> "): " <> show n
        Just x -> return x
{-# INLINE parseNonce2 #-}

-- | Compose Nonce1 (pool) and Nonce2 (mining client) to form the final
-- nonce.
--
-- Nonces are stored as Word64 and are injected into the block header in little
-- endian binary encoding. They are sent to the client in big endian hexadecimal
-- encoding.
--
composeNonce :: Nonce1 -> Nonce2 -> Nonce
composeNonce (Nonce1 (NonceSize s1) (Nonce n1)) (Nonce2 (NonceSize s2) (Nonce n2))
    | s1 + s2 == 8 = Nonce $ shiftL n2 (8 * int s1) + n1
    | otherwise = error $ "composeNonce: invalid size of combined nonce"
        <> "; size of nonce1: " <> show s1
        <> "; size of nonce2: " <> show s2
{-# INLINE composeNonce #-}

-- -------------------------------------------------------------------------- --
-- Parameter Types

-- | The mining agent of the client. For Kadena pools this can be any string.
--
newtype Agent = Agent T.Text
    deriving (Eq, Ord)
    deriving newtype (Show, A.ToJSON, A.FromJSON)

-- | Kadena Pools expect the user name to be the miner key. An optional worker
-- id can be appended with a dot as separator. It allows users to to identify
-- shares/rewards with workers in the statistics of the pool.
--
newtype Username = Username T.Text
    deriving (Eq, Ord)
    deriving newtype (Show, A.ToJSON, A.FromJSON)

-- | This is currently ignored by all Kadena Pools.
--
newtype Password = Password T.Text
    deriving (Eq, Ord)
    deriving newtype (Show, A.ToJSON, A.FromJSON)

-- | The Identifier for a job that is sent to the client. It is used to match a
-- nonce that is submitted by the client with a work header.
--
newtype JobId = JobId Int
    deriving (Eq, Ord)
    deriving newtype (Hashable)
    deriving (Show, A.ToJSON, A.FromJSON) via (IntHexText Int)

noJobId :: JobId
noJobId = JobId (-1)

nextJobId :: JobId -> JobId
nextJobId (JobId i) = JobId (i + 1)
{-# INLINE nextJobId #-}

-- | A string that identifies the client mining device. It is submited with
-- shares and pools my keep a record for the user to identify what device mined
-- shares.
--
newtype ClientWorker = ClientWorker T.Text
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- -------------------------------------------------------------------------- --
-- Requests

data MiningRequest
    = Subscribe MsgId (Agent, Static 'Nothing)
        -- ^ Subscribe
        --
        -- * params: @["agent", null]@
        -- * result: @[null, "nonce1", "nonce2 size"]@
        --
        -- nonce_1 is first part of the block header nonce in hex.
        --
        -- request:
        --
        -- @
        -- {
        --   "id": 1,
        --   "method": "mining.subscribe",
        --   "params": ["kdaminer-v1.0.0", null]
        -- }
        -- @
        --
        -- response:
        --
        -- @
        -- {
        --   "id": 1,
        --   "result": [null, "012345", 5],
        --   "error": null
        -- }
        -- @
        --
        -- Note: BM-K1 omits the @null@ parameter
        --
    | Authorize MsgId (Username, Password)
        -- ^ Authorize
        --
        -- * params: @["username.worker", "password"]@
        -- * result: @true@
        --
        -- @username@ is the wallet address. Worker id is used to identify the
        -- device. It is not clear to what extend different pools support the
        -- addition of the worker to the username in this message.
        --
        -- @password@ is ignored by all Kadena pools.
        --
        -- request:
        --
        -- @
        -- {
        --   "id": 2,
        --   "method": "mining.authorize",
        --   "params": ["900703b6dd2493696068af72957a94129e54e85f269becc665672bf4730fc6a3", "x"]
        -- }
        -- @
        --
        -- response:
        --
        -- @
        -- {
        --   "id": 2,
        --   "result": true,
        --   "error": null
        -- }
        -- @
        --
    | Submit MsgId (Username, ClientWorker, JobId, Nonce2)
        -- ^ Submit
        --
        -- * params: @["username.worker", "jobId", "nonce2"]@
        -- * result: @true / false@
        --
        -- request:
        --
        -- @
        -- {
        --   "id": 102,
        --   "method": "mining.submit",
        --   "params": [
        --     "900703b6dd2493696068af72957a94129e54e85f269becc665672bf4730fc6a3.worker1",
        --     "1234",
        --     "6789abcdef"
        --   ]
        -- }
        -- @
        --

deriving instance Show MiningRequest

instance A.ToJSON MiningRequest where
    toEncoding = A.pairs . mconcat . \case
        Subscribe mid params -> requestProperties "mining.subscribe" params (Just mid)
        Authorize mid params -> requestProperties "mining.authorize" params (Just mid)
        Submit mid params -> requestProperties "mining.submit" params (Just mid)
    toJSON = A.object . \case
        Subscribe mid params -> requestProperties "mining.subscribe" params (Just mid)
        Authorize mid params -> requestProperties "mining.authorize" params (Just mid)
        Submit mid params -> requestProperties "mining.submit" (submitParams params) (Just mid)
      where
        submitParams (Username u, ClientWorker w, j, n) = (u <> "." <> w, j, n)
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

parseMiningRequest
    :: NonceSize
        -- ^ Nonce2 size
    -> A.Value
    -> A.Parser MiningRequest
parseMiningRequest n2s = A.withObject "MiningRequest" $ \o -> do
    mid <- (o A..: "id") :: A.Parser MsgId
    (o A..: "method") >>= \case
        "mining.subscribe" -> (Subscribe mid <$> parseSubscribeParams o) A.<?> A.Key "mining.subscribe"
        "mining.authorize" -> (Authorize mid <$> parseAuthorizeParams o) A.<?> A.Key "mining.authorize"
        "mining.submit" -> (Submit mid <$> parseSubmitParams o) A.<?> A.Key "mining.submit"
        m -> fail $ "unknown message type " <> m
  where
    parseSubmitParams o = do
        (uw, j, n) <- o A..: "params"
        n2 <- parseNonce2 n2s n
        let (u,w) = bimap Username ClientWorker $ T.break (== '.') uw
        return (u, w, j, n2)

    parseSubscribeParams o = o A..: "params"
        <|> ((, Static :: Static 'Nothing) . _getT1 <$> o A..: "params")
        <|> (\() -> (Agent "unknown", Static :: Static 'Nothing)) <$> o A..: "params"

    parseAuthorizeParams o = o A..: "params"
{-# INLINE parseMiningRequest #-}

-- -------------------------------------------------------------------------- --
-- Notification

data MiningNotification
    = SetTarget (T1 Target)
        -- ^ Set Target
        --
        -- * params: @["32 bytes target in big endian hex"]@
        --
        -- @
        -- {
        --   "id": null,
        --   "method": "mining.set_target",
        --   "params": ["0001000000000000000000000000000000000000000000000000000000000000"]
        -- }
        -- @
        --

    | Notify (JobId, Work, Bool)
        -- ^ Notify
        --
        -- * params: @["jobId", "header", cleanJob]@
        --
        -- @
        -- {
        --   "id": null,
        --   "method": "mining.notify",
        --   "params": [
        --     "1234",
        --     "286 bytes header in hex",
        --     true
        --   ]
        -- }
        -- @
        --

instance A.ToJSON MiningNotification where
    toEncoding = A.pairs . mconcat . \case
        SetTarget p -> requestProperties "mining.set_target" p Nothing
        Notify p -> requestProperties "mining.notify" p Nothing
    toJSON = A.object . \case
        SetTarget p -> requestProperties "mining.set_target" p Nothing
        Notify p -> requestProperties "mining.notify" p Nothing
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance A.FromJSON MiningNotification where
    parseJSON = A.withObject "MiningNotification" $ \o -> do
        (o A..: "method") >>= \case
            "mining.set_target" -> SetTarget <$> o A..: "params"
            "mining.notify" -> Notify <$> o A..: "params"
            m -> fail $ "unknown message type " <> m
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Respones

data MiningResponse
    = SubscribeResponse MsgId (Either Error (Nonce1, NonceSize))
    | AuthorizeResponse MsgId (Either Error ())
    | SubmitResponse MsgId (Either Error Bool)

deriving instance Show MiningResponse

subscribeResponse :: MsgId -> Nonce1 -> NonceSize -> MiningResponse
subscribeResponse mid n1 n2s = SubscribeResponse mid $ Right (n1, n2s)

subscribeError :: MsgId -> T.Text -> MiningResponse
subscribeError mid msg = SubscribeResponse mid (Left $ Error (2,msg, A.Null))

authorizeResponse :: MsgId -> MiningResponse
authorizeResponse mid = AuthorizeResponse mid (Right ())

authorizeError :: MsgId -> T.Text -> MiningResponse
authorizeError mid msg = AuthorizeResponse mid (Left $ Error (1, msg, A.Null))

submitResponse :: MsgId -> Bool -> MiningResponse
submitResponse mid b = SubmitResponse mid (Right b)

submitError :: MsgId -> T.Text -> MiningResponse
submitError mid msg = SubmitResponse mid (Left $ Error (3, msg, A.Null))

instance A.ToJSON MiningResponse where
    toEncoding = A.pairs . mconcat . \case
        SubscribeResponse mid r -> responseProperties mid (subscribeReponseParams r)
        AuthorizeResponse mid r -> responseProperties mid r
        SubmitResponse mid r -> responseProperties mid r
    toJSON = A.object . \case
        SubscribeResponse mid r -> responseProperties mid (subscribeReponseParams r)
        AuthorizeResponse mid r -> responseProperties mid r
        SubmitResponse mid r -> responseProperties mid r
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

subscribeReponseParams :: Either Error (Nonce1, NonceSize) -> Either Error (Static 'Nothing, Nonce1, NonceSize)
subscribeReponseParams (Right (n1, ns)) = Right (StaticNull, n1, ns)
subscribeReponseParams (Left e) = Left e

-- | FIXME parse nonce in subscribe response
--
parseMiningResponse
    :: (MsgId -> MiningRequest)
        -- ^ Lookup for matching MiningRequests
    -> A.Value
    -> A.Parser (MiningRequest, MiningResponse)
parseMiningResponse pendingRequests = A.withObject "MiningResponse" $ \o -> do
    mid <- (o A..: "id") :: A.Parser MsgId
    case pendingRequests mid of
        r@Subscribe{} -> (r,) . SubscribeResponse mid <$> parseResponse' parseSubscribeParams o
        r@Authorize{} -> (r,) . AuthorizeResponse mid <$> parseResponse' parseAuthorizeParams o
        r@Submit{} -> (r,) . SubmitResponse mid <$> parseResponse o

  where
    parseSubscribeParams = A.parseJSON >=> \(StaticNull, v, s) -> case nonceSize @Int s of
        Nothing -> fail $ "invalid nonce2 size. Expected a value between 0 and 8 but got " <> show s
        Just ns -> (, ns) <$> parseNonce1 (complementNonceSize ns) v

    parseAuthorizeParams = A.parseJSON >=> \(T1 StaticTrue) -> return ()
{-# INLINE parseMiningResponse #-}

