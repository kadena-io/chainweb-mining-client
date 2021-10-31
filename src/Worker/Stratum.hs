{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Worker.Stratum
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Kadena Stratum Mining Protocol
--
-- cf. https://gist.github.com/mightybyte/f1567c2bec0380539c638225fb8c1cf4
--
-- TODO
--
-- * Create datatype for Error codes
-- * Find out if there is some standard for reporting errors
-- * proper reporting of discarded and stale shares (what are the precise modes)
--
-- Open Questions
-- * is authorizaiton required or optional (what are possible results)
-- * meaning of result of submit
-- * precise meaning notify 'clear' field
--
module Worker.Stratum
( withStratumServer
, submitWork

-- * Useful for implementing Clients
, parseMiningResponse

-- * Internal
, Nonce1(..)
, NonceSize(..)
, Nonce2(..)
, composeNonce
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.ByteString as P
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe
import Data.Streaming.Network
import qualified Data.Text as T

import qualified Streaming.Prelude as S

import System.IO.Unsafe
import qualified System.LogLevel as L

-- internal modules

import JsonRpc

import Logger

import Utils

import Worker

import WorkerUtils

-- -------------------------------------------------------------------------- --
-- Nonces

-- | Size of a nonce in bytes. This is a number between 0 and 8.
--
newtype NonceSize = NonceSize Int
    deriving (Show, Eq, Ord, Integral, Real, Enum, Num)
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
nonce1 s n@(Nonce w)
    | w < 2 ^ (s * 8) = Just $ Nonce1 s n
    | otherwise = Nothing
{-# INLINE nonce1 #-}

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
nonce2 s n@(Nonce w)
    | w < 2 ^ (s * 8) = Just $ Nonce2 s n
    | otherwise = Nothing
{-# INLINE nonce2 #-}

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
composeNonce (Nonce1 s1 (Nonce n1)) (Nonce2 s2 (Nonce n2))
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

parseMiningRequest :: NonceSize -> A.Value -> A.Parser MiningRequest
parseMiningRequest nonce2Size = A.withObject "MiningRequest" $ \o -> do
    mid <- (o A..: "id") :: A.Parser MsgId
    (o A..: "method") >>= \case
        "mining.subscribe" -> (Subscribe mid <$> parseSubscribeParams o) A.<?> A.Key "mining.subscribe"
        "mining.authorize" -> (Authorize mid <$> parseAuthorizeParams o) A.<?> A.Key "mining.authorize"
        "mining.submit" -> (Submit mid <$> parseSubmitParams o) A.<?> A.Key "mining.submit"
        m -> fail $ "unknown message type " <> m
  where
    parseSubmitParams o = do
        (uw, j, n) <- o A..: "params"
        n2 <- parseNonce2 nonce2Size n
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
    parseSubscribeParams = A.parseJSON >=> \(StaticNull, v, s) ->
        (,s) <$> parseNonce1 (8 - s) v

    parseAuthorizeParams = A.parseJSON >=> \(T1 StaticTrue) -> return ()
{-# INLINE parseMiningResponse #-}

-- -------------------------------------------------------------------------- --
-- Clients and Shares

-- | 16 bit allows for 65536 mining clients
--
-- nonce1Size :: NonceSize
-- nonce1Size = 4

data PoolCtx = PoolCtx
    { _workerAuthorization :: Authorize
    }

-- | A share is a solution of a job with respect to the session target.
--
-- Shares are the basis for paying out miners. This structure stores all
-- information about a share that is needed for a pool to implement differnt
-- payment methods.
--
data Share = Share
    { _shareId :: !JobId
    , _shareTarget :: !Target
    , _shareWork :: !Work
        -- ^ work with nonce of the share
    , _shareMiner :: !Username
    , _shareWorkerId :: !ClientWorker
    , _shareIsBlock :: !Bool
    }

data MiningClient = MiningClient
    { _miningClientNonce1 :: !Nonce1
    , _miningClientCurrentTarget :: !Target
    , _miningClientShares :: !Share
    }

-- authorize :: Username -> WorkerId -> Password -> IO Bool
-- authorize = error "TODO"

-- -------------------------------------------------------------------------- --
-- Intialize Global Stratum Worker

-- | TODO: make this configurable
--
host :: HostPreference
host = "*"

-- | TODO: make this configurable
--
port :: Int
port = 1917

type Authorize = Username -> Password -> IO (Either T.Text ())

-- | TODO distinguish between job and share:
--
-- Job: global from worker, solution for block
-- Share: session, solution for share target
--
data Job = Job
    { _jobId :: !JobId
    , _jobTarget :: !Target
    , _jobWork :: !Work
    , _jobResult :: !(MVar Nonce)
    }

noopJob :: Job
noopJob = unsafePerformIO $ Job (JobId (-1)) (Target "") (Work "") <$> newEmptyMVar
{-# NOINLINE noopJob #-}

-- | Stratum Server Context
--
-- A stratum server distributes work to clients. Solved work is recorded in
-- shares and the server keeps track of those.
--
-- Work is distributed in a non-overlapping way by assigning clients nonce
-- ranges. A nonce range is defined through an nonce prefix. The nonce prefix
-- is assigned during subscription.
--
-- The difficulty of a share is defined via a target.
--
-- Targets are adjusted from time to time and are the basis for measuring the
-- difficulty of the work that determines the value of share.
--
-- JobIds are used to match solutions (nonces) to the respective work work
-- items.
--
data StratumServerCtx = StratumServerCtx
    { _ctxAuthorizeFn :: !Authorize
        -- ^ function for user authentication and authorization. This sets
        -- the public key for paying out the mining rewards for a session.
        --
        -- Kadena pools don't use passwords and the scope of authorization is
        -- the stratum session.
        --
        -- For bookkeeping it is also possible to associate a device/worker id
        -- with the shares from a session.

    , _ctxJobs :: !(TVar (HM.HashMap JobId Job))
        -- ^ there might be some contention on this variable, but we don't care
        -- too much. It is modified only by the workers, which access it for
        -- each new work item that they get. That's roughly the block rate
        -- time the numbers of worker. The latter number should be small.
        -- (there's little benefit in having a larger number)
        --
        -- It is read by all sessions using readTVarIO. If a job disappears
        -- while processing the share, the share is still counted, even if the
        -- block is solved in between. So, the time of the `readTVarIO` of the
        -- job marks the point in time until a share must be found. (we could
        -- change that by doing another read + job lookup when the share is
        -- recorded)
        --
        -- Solutions are synchronized on the result MVar within a job.
        -- Contention on that is much lower, since only valid solution are
        -- submitted to that variable.

    , _ctxCurrentJob :: !(TVar Job)
        -- ^ the most recent job. This is used as basis for the job stream
        -- of each session. There is no guarantee that each session sees each
        -- job. Actually, it may even make sense for sessions to ignore some of
        -- these in order to reduce the number of notifications that are send to
        -- the client.

    , _ctxCurrentId :: !(IORef Int)
        -- ^ Ticket counter for job ids.
    }

newStratumServerCtx :: IO StratumServerCtx
newStratumServerCtx = StratumServerCtx
    (\_ _ -> return (Right ()))
    <$> newTVarIO mempty
    <*> newTVarIO noopJob
    <*> newIORef 0

-- -------------------------------------------------------------------------- --
-- Worker Interface

-- | This is called from the mining loop of chainweb-mining-client:
--
-- It is recommended to start several worker threads, so that there are always
-- enough active work items available.
--
submitWork :: StratumServerCtx -> Logger -> Nonce -> Target -> Work -> IO Work
submitWork ctx l _nonce target work =  withLogTag l "Stratum Worker" $ \logger -> do
    mask $ \umask -> do
        job <- umask $ newJob logger ctx target work
        flip onException (writeLog logger L.Info ("discarded unfinished job"  <> sshow (_jobId job))) $
            flip finally (removeJob ctx (_jobId job)) $ umask $ do
                checkJob logger job
      where
        -- Check that the solution for a job is correct. This should never fail.
        -- Sessions should only submit shares that are actually solving the
        -- block.
        checkJob logger job = do
            nonce <- takeMVar (_jobResult job) -- at this point the mvar is available again
            !w <- injectNonce nonce (_jobWork job)
            fastCheckTarget (_jobTarget job) w >>= \case
                True -> do
                    writeLog logger L.Info $ "submitted job " <> sshow (_jobId job)
                    return w
                False -> do
                    writeLog logger L.Error $ "rejected job: invalid result " <> sshow (_jobId job)
                    writeLog logger L.Info $ "invalid nonce: " <> sshow nonce
                        <> ", target: " <> sshow (_jobTarget job)
                        <> ", job work: " <> sshow (_jobWork job)
                        <> ", result work: " <> sshow w
                        <> ". Continue with job"
                    checkJob logger job

newJob :: Logger -> StratumServerCtx -> Target -> Work -> IO Job
newJob logger ctx target work = do

    -- Create new job
    jid <- JobId <$> atomicModifyIORef' (_ctxCurrentId ctx) (\x -> (x + 1, x))
    job <- Job jid target work <$> newEmptyMVar

    flip onException (removeJob ctx jid) $ do

        -- add job to the job table
        atomically $ modifyTVar' (_ctxJobs ctx) $ HM.insert jid job

        -- notify all active connections
        -- (no need to do this in the same tx as above)
        atomically $ writeTVar (_ctxCurrentJob ctx) job

        writeLog logger L.Info $ "created new job " <> T.pack (show (_jobId job))
        return job

removeJob :: StratumServerCtx -> JobId -> IO ()
removeJob ctx jid = atomically $ modifyTVar' (_ctxJobs ctx) $ HM.delete jid

-- -------------------------------------------------------------------------- --
-- Sessions

-- | ASICs are powerful machines. When connected to a network with low
-- difficulty it may produce a large amount of shares that can overload the
-- network, the stratum server, or the device itself. For that reason we set a
-- minimum target -- even if it means that the client does more work than needed
-- to solve a single block. In the end, producing blocks faster than they can be
-- processed down stream has no benefit and can even be harmful.
--
-- TODO: this should be configurable (possibly also on a per agent base)
--
minTarget :: Target
minTarget = "0000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff"

initialTarget :: Target
initialTarget = minTarget

defaultNonce1 :: Nonce1
defaultNonce1 = Nonce1 2 (Nonce 0xffff)

defaultNonce2Size :: NonceSize
defaultNonce2Size = NonceSize (8 - s)
  where
    (Nonce1 (NonceSize s) _) = defaultNonce1

data SessionState = SessionState
    { _sessionNonce1 :: !Nonce1
    , _sessionTarget :: !(TVar Target)
    , _authorized :: !(TMVar Username)
    , _subscribed :: !(TMVar Agent)
    }

sessionNonce2Size :: SessionState -> NonceSize
sessionNonce2Size s = let Nonce1 s1 _ = _sessionNonce1 s in 8 - s1

data AppResult
    = JsonRpcError T.Text
    | StratumError T.Text
    | TimeoutError T.Text
    | ConnectionClosed T.Text
    deriving (Show, Eq, Ord)

instance Exception AppResult

type RequestStream = S.Stream (S.Of MiningRequest) IO AppResult

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a = tryTakeTMVar var >> putTMVar var a

session :: Logger -> StratumServerCtx -> AppData -> IO ()
session l ctx app = withLogTag l "Stratum Session" $ \l2 -> withLogTag l2 (sshow (appSockAddr app)) $ \logger ->
    flip finally (appCloseConnection app) $ do
        sessionCtx <- initState

        -- Run Request Stream and Job Stream
        r <- race (processRequests logger sessionCtx) $ do
            awaitSubscribe sessionCtx
            S.mapM_ (notify logger app sessionCtx) jobStream

        case r of
            Right _ -> writeLog logger L.Error "Stratum session ended unexpectedly because an internal chainweb-mining-client issue"
            Left e@(JsonRpcError msg) -> do
                replyError app $ Error (-32700, "Parse Error", A.String msg)
                writeLog logger L.Warn $ "session termianted with " <> sshow e
            Left e@(StratumError msg) -> do
                replyError app $ Error (-32600, "Invalid Request", A.String msg)
                    -- FIXME be more specific (cf. json rpc internal error codes)
                writeLog logger L.Warn $ "session termianted with " <> sshow e
            Left e@(TimeoutError msg) -> do
                replyError app $ Error (-1, "Request Timeout", A.String msg)
                    -- TODO not yet implemented
                writeLog logger L.Warn $ "session termianted with " <> sshow e
            Left e@(ConnectionClosed msg) -> do
                replyError app $ Error (-2, "Connection Error", A.String msg)
                    -- TODO: is the connection actually closed if we receive this?
                    -- TODO: if the connection got closed we can't reply
                writeLog logger L.Warn $ "session termianted with " <> sshow e

  where

    authorize = _ctxAuthorizeFn ctx

    awaitSubscribe sctx = atomically $ void $ takeTMVar (_subscribed sctx)

    -- Should we rate limit this stream by randomly skipping blocks?
    --
    jobStream :: S.Stream (S.Of Job) IO ()
    jobStream = do
        cur <- liftIO $ readTVarIO (_ctxCurrentJob ctx)
        go cur
      where
        go :: Job -> S.Stream (S.Of Job) IO ()
        go old = do
            new <- liftIO $ atomically $ do
                cur <- readTVar (_ctxCurrentJob ctx)
                if _jobId old == _jobId cur
                    then retry
                    else return cur
            S.yield new
            go new

    -- For now we let the ASIC start at 0
    initState = SessionState defaultNonce1 <$> newTVarIO initialTarget <*> newEmptyTMVarIO <*> newEmptyTMVarIO

    processRequests :: Logger -> SessionState -> IO AppResult
    processRequests logger sessionCtx = S.mapM_ (handleRequest logger sessionCtx) (messages sessionCtx app)

    handleRequest :: Logger -> SessionState -> MiningRequest -> IO ()
    handleRequest logger sessionCtx = \case

        -- AUTHORIZE
        (Authorize mid (user, pwd)) -> do
            authorize user pwd >>= \case
                Right () -> reply app $ authorizeResponse mid
                Left err -> reply app $ authorizeError mid err
            atomically $ writeTMVar (_authorized sessionCtx) user

        -- SUBSCRIBE
        (Subscribe mid (agent, _)) -> do
            reply app $ subscribeResponse mid (_sessionNonce1 sessionCtx) defaultNonce2Size
            atomically $ writeTMVar (_subscribed sessionCtx) agent

        -- SUBMIT
        (Submit mid (_u, _w, j, n2)) -> withLogTag logger ("job-" <> sshow j) $ \jlog -> do

            -- Do all checks before obtaining the lock on the job
            readTVarIO (_ctxJobs ctx) >>= \m -> case HM.lookup j m of

                -- Inactive job: Discard stale share
                Nothing -> do
                    writeLog jlog L.Info "Discarded stale share"
                    reply app $ SubmitResponse mid (Right False) -- FIXME is this the correct response?

                -- Active Job: Check share
                Just job -> do

                    let n = composeNonce (_sessionNonce1 sessionCtx) n2

                    -- Check if share is valid (matches share target)
                    finalWork <- injectNonce n (_jobWork job)
                    st <- readTVarIO (_sessionTarget sessionCtx)
                    fastCheckTarget st finalWork >>= \case

                        -- Invalid Share:
                        False -> do
                            writeLog jlog L.Warn "got invalid nonce"
                            writeLog jlog L.Info $ "invalid nonce"
                                <> "; nonce2:" <> sshow n2
                                <> "; nonce: " <> sshow n
                                <> "; work: " <> sshow (_jobWork job)
                                <> "; final work: " <> sshow finalWork
                                <> "; target: " <> sshow (_jobTarget job)
                                <> "; session target: " <> sshow st
                            reply app $ SubmitResponse mid (Left $ Error (31, "invalid nonce", A.Null)) -- FIXME is this correct reponse?

                        -- Valid Share:
                        True -> do
                            -- We've got a valid share
                            --
                            -- TODO: record share in the Pool Context
                            writeLog jlog L.Info $ "got valid share: nonce2:" <> sshow n2 <> "; nonce: " <> sshow n

                            -- Check whether it is a solution for the job and
                            -- only submit if it is. We do this here in order to
                            -- fail early and avoid contention on the job result
                            -- MVar.

                            -- TODO we could save a few CPU cycles by reusing the hash
                            -- from the previous check
                            fastCheckTarget (_jobTarget job) finalWork >>= \case
                                False -> reply app $ SubmitResponse mid (Right True)
                                True -> do
                                    writeLog jlog L.Info $ "solved block: nonce2:" <> sshow n2 <> "; nonce: " <> sshow n
                                    -- Yeah, we've solved a block
                                    -- Commit final result to job
                                    void $ tryPutMVar (_jobResult job) n
                                    reply app $ SubmitResponse mid (Right True)

send :: A.ToJSON a => AppData -> a -> IO ()
send app a = appWrite app (BL.toStrict $ A.encode a) >> appWrite app "\n"

replyError :: AppData -> Error -> IO ()
replyError = send

reply :: AppData -> MiningResponse -> IO ()
reply = send

notify :: Logger -> AppData -> SessionState -> Job -> IO ()
notify logger app sessionCtx job = do

    -- FIXME set target only if needed. Find out whether changing target slows
    -- down the ASICs If it is expensive we'll have to implement mining of
    -- shares.
    --
    -- TODO: the pool context should provide guidance what session target
    -- should be used. Something ala
    --
    -- updateSessionTarget
    --  :: PoolCtx
    --  -> SessionCtx
    --  -> Target
    --      -- ^ globally configured minimum target
    --  -> Target
    --      -- ^ job target
    --  -> IO (Maybe Target)
    --      -- ^ updated target
    --
    let t =  min minTarget (_jobTarget job)

    writeLog logger L.Info $ "settting session target: " <> sshow t
    send app $ SetTarget $ T1 t
    atomically $ writeTVar (_sessionTarget sessionCtx) t
        -- Note, that there is a small chance of a race here, if the device is really fast
        -- and returns a solution before this is updated. We could solve that by
        -- making the target a TMVar or MVar and taking it before we send the
        -- "mining.set_target".
        --
        -- Most likely target changes are minor and shares are accepted even in
        -- case of a race.

    writeLog logger L.Info "sending notification"
    send app $ Notify (_jobId job, _jobWork job, True) -- for now we always replace previous wor


-- | TODO: we probably need some protection against clients keeping
-- connections open without making progress.
--
-- * limit size of messages and fail parsing if the limit is exeeced
-- * add timeout within parsing of a single message
-- * add timeout for time between messages?
--
--
messages :: SessionState -> AppData -> RequestStream
messages sessionCtx app = go mempty
  where
    nonce2Size = sessionNonce2Size sessionCtx

    go :: B.ByteString -> S.Stream (S.Of MiningRequest) IO AppResult
    go i = P.parseWith (liftIO (appRead app)) A.json' i >>= \case
        P.Fail _ path err ->
            return $ JsonRpcError $ "Failed to parse JSON RPC message: " <> T.pack err <> " " <> sshow path
        P.Partial _cont ->
            -- Can this actually happen, or would it a P.Fail?
            return $ ConnectionClosed "Connection closed unexpectedly"
        P.Done i' val -> case A.parse (parseMiningRequest nonce2Size) val of
            A.Error err ->
                return $ StratumError $ "Unrecognized message: " <> T.pack err <> ". " <> sshow val
            A.Success result -> do
                S.yield result
                go i'

-- -------------------------------------------------------------------------- --
-- Stratum Server

withStratumServer :: Logger -> (StratumServerCtx -> IO ()) -> IO ()
withStratumServer l inner = withLogTag l "Stratum Server" $ \logger -> do
    ctx <- newStratumServerCtx
    race (server logger ctx) (inner ctx) >>= \case
        Left _ -> writeLog logger L.Error "server exited unexpectedly"
        Right _ -> do
            writeLog logger L.Error "mining loop existed unexpectedly"
            return ()
  where
    server logger ctx = flip finally (writeLog logger L.Info "server stopped") $ do
        writeLog logger L.Info "Start stratum server"
        runTCPServer (serverSettingsTCP port host) (session logger ctx)

-- -------------------------------------------------------------------------- --
-- Example sessions

-- $ telnet kda.f2pool.com 5400
-- Trying 47.111.206.170...
-- Connected to kda.f2pool.com.
-- Escape character is '^]'.
-- { "id":1, "method": "mining.authorize", "params":["136029d4a40642de75bebca833ea150fc027e08fbedd29c943aab87810e77378", ""] }
-- {"id":1,"result":true,"error":null}
-- { "id":2, "method": "mining.subscribe", "params":["my-miner", null] }
-- {"id":2,"result":[null,"09e8",6],"error":null}
-- {"id":null,"method":"mining.set_target","params":["0000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffff"]}
-- {"id":null,"method":"mining.notify","params":["6412800009e8","00000000000000001128f13f5ccf050092f4573bc51d0fcf58a3729d3d4f57f9f279bbb50c793a6446fcfd226c8a4c1d0300080000003903c8b45da2ceaf0fab587860f7868ab29ea35fc4ac16796f7dcba43749f86b0d000000e77649998da94d3be41ad1e8d4038c0d326680a7ee9fdbaccb98f98ac25f7fe512000000d88e2cf14591a36a52bf31f1176f5306a196e95dde5e18abf6af306442a402f1019fd29b54b24d0c484656cd24bc0ba6aade86888e889f9c100100000000000014e70eccbc36b15e8960c845a895817f9cc3722059a51a233eb4839ffaa5824503000000b44b93673c8f1b633403000000000000000000000000000000000000000000006e0620000000000005000000648c1c745bcf05000000000000000000",true]}
-- {"id":null,"method":"mining.notify","params":["645a800809e8","0000000000000000f4450a405ccf0500d8c9800d1a2021618d83f88b5d1b3d21fd068d40e73676fb3b8a164d8878d51e0300010000002f4fa19a2ddbc10ffb1e6076e6ca881fbd254e109b7490ac6362c8ce371e501d08000000b8b8d54da5d10f79b9920dce6eb1884ad9cedf85640eea2ef5164fea8de08a4c09000000423e846c73a0d1f7f002bb34de2ba9b4559dea999c8a00a94caa84408cf87ceff9e53f39b6b03dbb6e67fe63d1659afc888d4842049ac7260f01000000000000dcc0c0a68efe1a30ea8d72f4a748a46b7cc1ded1d5cc5932d7709c1b9f16abd606000000f37dbe0df9aac5663403000000000000000000000000000000000000000000006d0620000000000005000000615dc5725bcf05000000000000000000",true]}

-- $ telenet kda.ss.poolflare.com 443
-- Trying 192.99.233.13...
-- Connected to kda.ss.poolflare.com.
-- Escape character is '^]'.
-- { "id":1, "method": "mining.authorize", "params":["136029d4a40642de75bebca833ea150fc027e08fbedd29c943aab87810e77378", ""] }
-- {"id":1,"result":true,"error":null}
-- { "id":2, "method": "mining.subscribe", "params":["my-miner", null] }
-- {"id":2,"result":[null,"a796",6],"error":null}
-- {"id":null,"method":"mining.set_target","params":["0000000000400000000000000000000000000000000000000000000000000000"]}
-- {"id":null,"method":"mining.notify","params":["5d064e41","0000000000000000ab87cfaf5ccf050098d107d0470e4de368c92aa0235e190e67cc5c7cb9ee9c485439b51c566fc249030008000000608351babdabc848f90ad4891044462291d652e390a1dd6ecbb44ab733a33dce0d000000377b67dfa6df2f33f3edcb784cf3044ceee29fefaef0b90a69996c91b874c80812000000339a23f547d271eb4a68c0761b16bb5d558a1cd927a57312ece1f5ea7381f839693e8cb25bc71cc2bcb4ef84fe0119f21300b83ab768a44105010000000000002491767e21e31c67eef18cf5e8a704dc936e3a98b026f2175e672b506fa9068203000000bfde81ee849ecca0340300000000000000000000000000000000000000000000ad0620000000000005000000e243b7415ccf05000000000000000000"]}
-- {"id":null,"method":"mining.notify","params":["02040844","0000000000000000ac87cfaf5ccf050098d107d0470e4de368c92aa0235e190e67cc5c7cb9ee9c485439b51c566fc249030008000000608351babdabc848f90ad4891044462291d652e390a1dd6ecbb44ab733a33dce0d000000377b67dfa6df2f33f3edcb784cf3044ceee29fefaef0b90a69996c91b874c80812000000339a23f547d271eb4a68c0761b16bb5d558a1cd927a57312ece1f5ea7381f839693e8cb25bc71cc2bcb4ef84fe0119f21300b83ab768a44105010000000000002491767e21e31c67eef18cf5e8a704dc936e3a98b026f2175e672b506fa9068203000000bfde81ee849ecca0340300000000000000000000000000000000000000000000ad0620000000000005000000e243b7415ccf05000000000000000000"]}
-- {"id":null,"method":"mining.notify","params":["243fc4d7","0000000000000000a298c5b05ccf050098d107d0470e4de368c92aa0235e190e67cc5c7cb9ee9c485439b51c566fc249030008000000608351babdabc848f90ad4891044462291d652e390a1dd6ecbb44ab733a33dce0d000000377b67dfa6df2f33f3edcb784cf3044ceee29fefaef0b90a69996c91b874c80812000000339a23f547d271eb4a68c0761b16bb5d558a1cd927a57312ece1f5ea7381f839693e8cb25bc71cc2bcb4ef84fe0119f21300b83ab768a44105010000000000002491767e21e31c67eef18cf5e8a704dc936e3a98b026f2175e672b506fa9068203000000bfde81ee849ecca0340300000000000000000000000000000000000000000000ad0620000000000005000000e243b7415ccf05000000000000000000"]}


