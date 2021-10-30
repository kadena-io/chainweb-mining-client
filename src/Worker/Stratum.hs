{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
-- Open Questions:
--
-- * error codes
-- * is authorizaiton required or optional (what are possible results)
-- * meaning of agent
-- * meaning of worker
-- * meaning of result of submit
-- * meaning notify 'clear' field
-- * how long are jobids retained
--
module Worker.Stratum
( withStratumServer
, submitWork

-- * Useful for implementing Clients
, parseMiningResponse
) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Streaming.Network
import Data.String
import qualified Data.Text as T
import Data.Word

import Foreign.Ptr
import Foreign.Storable

import Numeric.Natural

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
-- Parameter Types

-- | The mining agent of the client. For Kadena pools this can be any string.
--
newtype Agent = Agent T.Text
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- | The most significant bytes of the nonce. Thse are set by the pool.
--
-- The nonce bytes are injected into the work header in little endian encoding:
-- @Nonce2 <> Nonce1@
--
-- In stratum messages nonce bytes are encoding as hex strings with big endian
-- byte order.
--
newtype Nonce1 = Nonce1 BS.ShortByteString
    deriving (Show, Eq, Ord)
    deriving (A.ToJSON, A.FromJSON) via ReversedHexEncodedShortByteString

-- | The least significant bytes of the nonce. These respresent the share of
-- work performed by the client.
--
-- The nonce bytes are injected into the work header in little endian encoding:
-- @Nonce2 <> Nonce1@
--
-- In stratum messages nonce bytes are encoding as hex strings with big endian
-- byte order.
--
newtype Nonce2 = Nonce2 BS.ShortByteString
    deriving (Show, Eq, Ord)
    deriving (A.ToJSON, A.FromJSON) via ReversedHexEncodedShortByteString

-- | TODO: check length of nonce2
--
composeNonce :: Nonce1 -> Nonce2 -> Nonce
composeNonce (Nonce1 n1) (Nonce2 n2) = Nonce $! unsafePerformIO $
    BS.useAsCStringLen (n2 <> n1) $ \(ptr, _) -> peek @Word64 (castPtr ptr)
        -- FIXME make sure this is LE

{-# INLINE composeNonce #-}

-- | The size of the @Nonce2@ in bytes.
--
newtype Nonce2Size = Nonce2Size Natural
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- | Kadena Pools expect the user name to be the miner key. An optional worker
-- id can be appended with a dot as separator. It allows users to to identify
-- shares/rewards with workers in the statistics of the pool.
--
newtype Username = Username T.Text
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- | This is currently ignored by all Kadena Pools.
--
newtype Password = Password T.Text
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- | The Identifier for a job that is sent to the client. It is used to match a
-- nonce that is submitted by the client with a work header.
--
newtype JobId = JobId Int
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable)
    deriving (A.ToJSON, A.FromJSON) via (HexInt Int)

-- | A string that identifies the client mining device. It is submited with
-- shares and pools my keep a record for the user to identify what device mined
-- shares.
--
newtype ClientWorker = ClientWorker T.Text
    deriving (Show, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

-- | Hex-Encoding of the work bytes that are sent to the mining client.
--
newtype WorkHeader = WorkHeader Work
    deriving (Show, Eq, Ord)
    deriving (A.ToJSON, A.FromJSON) via HexEncodedShortByteString

-- | Target in big endian hex representation
--
newtype StratumTarget = StratumTarget Target
    deriving (Show, Eq, Ord)
    deriving (A.ToJSON, A.FromJSON) via ReversedHexEncodedShortByteString

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

instance A.FromJSON MiningRequest where
    parseJSON = A.withObject "MiningRequest" $ \o -> do
        mid <- (o A..: "id") :: A.Parser MsgId
        (o A..: "method") >>= \case
            "mining.subscribe" -> Subscribe mid <$> o A..: "params" A.<?> A.Key "mining.subscribe"
            "mining.authorize" -> Authorize mid <$> o A..: "params" A.<?> A.Key "mining.authorize"
            "mining.submit" -> Submit mid . submitParams <$> o A..: "params" A.<?> A.Key "mining.submit"
            m -> fail $ "unknown message type " <> m
      where
        submitParams (uw, j, n) = let (u,w) = T.break (== '.') uw in (Username u, ClientWorker w, j, n)
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Notification

data MiningNotification
    = SetTarget StratumTarget
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

    | Notify (JobId, WorkHeader, Bool)
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
    = SubscribeResponse MsgId (Either Error (Static 'Nothing, Nonce1, Nonce2Size))
    | AuthorizeResponse MsgId (Either Error (Static 'True))
    | SubmitResponse MsgId (Either Error Bool)

deriving instance Show MiningResponse

subscribeResponse :: MsgId -> Nonce1 -> Nonce2Size -> MiningResponse
subscribeResponse mid n1 n2s = SubscribeResponse mid $ Right (Static, n1, n2s)

subscribeError :: MsgId -> T.Text -> MiningResponse
subscribeError mid msg = SubscribeResponse mid (Left $ Error (2,msg, A.Null))

authorizeResponse :: MsgId -> MiningResponse
authorizeResponse mid = AuthorizeResponse mid (Right Static)

authorizeError :: MsgId -> T.Text -> MiningResponse
authorizeError mid msg = AuthorizeResponse mid (Left $ Error (1, msg, A.Null))

submitResponse :: MsgId -> Bool -> MiningResponse
submitResponse mid b = SubmitResponse mid (Right b)

submitError :: MsgId -> T.Text -> MiningResponse
submitError mid msg = SubmitResponse mid (Left $ Error (3, msg, A.Null))

instance A.ToJSON MiningResponse where
    toEncoding = A.pairs . mconcat . \case
        SubscribeResponse mid r -> responseProperties mid r
        AuthorizeResponse mid r -> responseProperties mid r
        SubmitResponse mid r -> responseProperties mid r
    toJSON = A.object . \case
        SubscribeResponse mid r -> responseProperties mid r
        AuthorizeResponse mid r -> responseProperties mid r
        SubmitResponse mid r -> responseProperties mid r
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

parseMiningResponse
    :: (MsgId -> MiningRequest)
        -- ^ Lookup for matching MiningRequests
    -> A.Value
    -> A.Parser (MiningRequest, MiningResponse)
parseMiningResponse pendingRequests = A.withObject "MiningResponse" $ \o -> do
    mid <- (o A..: "id") :: A.Parser MsgId
    case pendingRequests mid of
        r@Subscribe{} -> (r,) . SubscribeResponse mid <$> parseResponse o
        r@Authorize{} -> (r,) . AuthorizeResponse mid <$> parseResponse o
        r@Submit{} -> (r,) . SubscribeResponse mid <$> parseResponse o
{-# INLINE parseMiningResponse #-}

-- -------------------------------------------------------------------------- --
-- Clients and Shares

newtype NonceSize = NonceSize Int

-- | 16 bit allows for 65536 mining clients
--
-- nonce1Size :: NonceSize
-- nonce1Size = 4

data PoolCtx = PoolCtx
    { _workerAuthorization :: Authorize
    }

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

host :: HostPreference
host = "*"

port :: Int
port = 1917

type Authorize = Username -> Password -> IO (Either T.Text ())

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

    , _ctxJobs :: !(MVar (HM.HashMap JobId Job))
    , _ctxCurrentJob :: !(TVar Job)
    , _ctxCurrentId :: !(IORef Int)
    }

newStratumServerCtx :: IO StratumServerCtx
newStratumServerCtx = StratumServerCtx
    (\_ _ -> return (Right ()))
    <$> newMVar mempty
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
                nonce <- takeMVar (_jobResult job)
                !w <- injectNonce nonce (_jobWork job)
                writeLog logger L.Info $ "submitted job " <> sshow (_jobId job)
                return w
                -- FIXME:
                -- * check the share target -> record share in pool context
                -- * check the block target -> submit solution to chain

newJob :: Logger -> StratumServerCtx -> Target -> Work -> IO Job
newJob logger ctx target work = do

    -- Create new job
    jid <- JobId <$> atomicModifyIORef' (_ctxCurrentId ctx) (\x -> (x + 1, x))
    job <- Job jid target work <$> newEmptyMVar

    flip onException (removeJob ctx jid) $ do

        -- add job to the job table
        modifyMVar (_ctxJobs ctx) $ \m -> let !r = HM.insert jid job m in return (r, ())

        -- notify all active connections
        atomically $ writeTVar (_ctxCurrentJob ctx) job

        writeLog logger L.Info $ "created new job " <> T.pack (show (_jobId job))
        return job

removeJob :: StratumServerCtx -> JobId -> IO ()
removeJob ctx jid =
    modifyMVar (_ctxJobs ctx) $ \m -> let !r = HM.delete jid m in return (r, ())

-- -------------------------------------------------------------------------- --
-- Sessions

data SessionState = SessionState
    { _sessionNonce1 :: !Nonce1
    , _authorized :: !(TMVar Username)
    , _subscribed :: !(TMVar Agent)
    }

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
        r <- race (processRequests sessionCtx) $ do
            awaitSubscribe sessionCtx
            S.mapM_ (notify app) jobStream
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
    initState = SessionState (Nonce1 "") <$> newEmptyTMVarIO <*> newEmptyTMVarIO

    processRequests :: SessionState -> IO AppResult
    processRequests sessionCtx = S.mapM_ (handleRequest sessionCtx) (messages app)

    handleRequest :: SessionState -> MiningRequest -> IO ()
    handleRequest sessionCtx = \case
        (Authorize mid (user, pwd)) -> do
            authorize user pwd >>= \case
                Right () -> reply app $ authorizeResponse mid
                Left err -> reply app $ authorizeError mid err
            atomically $ writeTMVar (_authorized sessionCtx) user

        (Subscribe mid (agent, _)) -> do
            reply app $ subscribeResponse mid (_sessionNonce1 sessionCtx) (Nonce2Size 8)
            atomically $ writeTMVar (_subscribed sessionCtx) agent

        (Submit mid (_u, _w, j, n2)) -> do

            -- Commit new to job
            modifyMVar (_ctxJobs ctx) $ \m -> case HM.lookup j m of
                Nothing -> return (m, ())
                Just job -> do
                    let n = composeNonce (_sessionNonce1 sessionCtx) n2
                    void $ tryPutMVar (_jobResult job) n
                    return (m, ())

            reply app $ SubmitResponse mid (Right True) -- FIXME do a check?

send :: A.ToJSON a => AppData -> a -> IO ()
send app a = appWrite app (BL.toStrict $ A.encode a) >> appWrite app "\n"

replyError :: AppData -> Error -> IO ()
replyError = send

reply :: AppData -> MiningResponse -> IO ()
reply = send

notify :: AppData -> Job -> IO ()
notify app job = do
    -- FIXME set target only if needed. Find out whether changing target slows down the ASICs
    -- If it is expensive we'll have to implement mining of shares.
    --
    send app $ SetTarget (StratumTarget $ _jobTarget job)
    send app $ Notify (_jobId job, WorkHeader (_jobWork job), True) -- for now we always replace previous wor


-- | TODO: we probably need some protection against clients keeping
-- connections open without making progress.
--
-- * limit size of messages and fail parsing if the limit is exeeced
-- * add timeout within parsing of a single message
-- * add timeout for time between messages?
--
--
messages :: AppData -> RequestStream
messages app = go mempty
  where
    go :: B.ByteString -> S.Stream (S.Of MiningRequest) IO AppResult
    go i = P.parseWith (liftIO (appRead app)) A.json' i >>= \case
        P.Fail _ path err ->
            return $ JsonRpcError $ "Failed to parse JSON RPC message: " <> T.pack err <> " " <> sshow path
        P.Partial _cont ->
            -- Can this actually happen, or would it a P.Fail?
            return $ ConnectionClosed "Connection closed unexpectedly"
        P.Done i' val -> case A.fromJSON val of
            A.Error err ->
                return $ StratumError $ "Unrecognized message: " <> T.pack err
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
        Right _ -> return ()
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


