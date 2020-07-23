{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A fake Chainweb POW miner for testing and simulations.
--
module Main
( main
) where

import Configuration.Utils hiding (Error)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (IOException, SomeAsyncException)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Word

import GHC.Generics

import qualified Network.Connection as HTTP
import Network.HostAddress
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.Wai.EventSource.EventStream
import Network.Wai.EventSource.Streaming

import Numeric.Natural

import PkgInfo

import qualified Streaming.Prelude as SP

import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- -------------------------------------------------------------------------- --
-- Orphans

instance ToJSON HostAddress where
    toJSON = toJSON . hostAddressToText
    {-# INLINE toJSON #-}

instance FromJSON HostAddress where
    parseJSON = withText "HostAddress"
        $ either (fail . show) return . hostAddressFromText
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
--  Utils

textReader :: (T.Text -> Either SomeException a) -> ReadM a
textReader p = eitherReader $ first show . p . T.pack

sshow :: Show a => IsString b => a -> b
sshow = fromString . show
{-# INLINE sshow #-}

-- -------------------------------------------------------------------------- --
-- Logging

logg :: LogLevel -> T.Text -> IO ()
logg l msg = T.putStrLn $ "[" <> sshow l <> "]" <> " " <> msg

logDebug, logInfo, logWarn, logError :: T.Text -> IO ()
logDebug = logg Debug
logInfo = logg Info
logWarn = logg Warn
logError = logg Error

-- -------------------------------------------------------------------------- --
-- HashRate

newtype HashRate = HashRate Double
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Read, Num, Fractional, Floating, Real, Enum, Hashable, ToJSON, FromJSON)

-- | Default is 1MH
defaultHashRate :: HashRate
defaultHashRate = 1_000_000

-- -------------------------------------------------------------------------- --
-- Miner

-- | The account name is that same as the public key. Different account names
-- are not supported.
--
-- Only a single base64UrlWithoutPadding encoded key may be used and the keyset
-- is "<".
--
newtype MinerPublicKey = MinerPublicKey T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

instance ToJSON MinerPublicKey where
    toJSON (MinerPublicKey k) = toJSON k
    {-# INLINE toJSON #-}

instance FromJSON MinerPublicKey where
    parseJSON = withText "MinerPublicKey" $ return . MinerPublicKey
    {-# INLINE parseJSON #-}
        -- TODO perform well-formedness checks

newtype Miner = Miner MinerPublicKey
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

instance ToJSON Miner where
    toJSON (Miner (MinerPublicKey k)) = object
        [ "account" .= k
        , "public-keys" .= [ k ]
        , "predicate" .= ("<" :: T.Text)
        ]

-- -------------------------------------------------------------------------- --
-- Configuration

newtype ChainwebVersion = ChainwebVersion T.Text
    deriving (Show, Read, Eq, Ord, Generic)
    deriving newtype (Hashable, ToJSON, FromJSON)

data Config = Config
    { _configHashRate :: !HashRate
    , _configNode :: !HostAddress
    , _configUseTls :: !Bool
    , _configInsecure :: !Bool
    , _configPublicKey :: !MinerPublicKey
    , _configThreadCount :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
    { _configHashRate = defaultHashRate
    , _configNode = unsafeHostAddressFromText "localhost:1789"
    , _configUseTls = True
    , _configInsecure = True
    , _configPublicKey = MinerPublicKey ""
    , _configThreadCount = 10
    }

instance ToJSON Config where
    toJSON c = object
        [ "hashRate" .= _configHashRate c
        , "node" .= _configNode c
        , "useTls" .= _configUseTls c
        , "insecure" .= _configInsecure c
        , "publicKey" .= _configPublicKey c
        , "threadCount" .= _configThreadCount c
        ]

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< configHashRate ..: "hashRate" % o
        <*< configNode ..: "node" % o
        <*< configUseTls ..: "useTls" % o
        <*< configInsecure ..: "insecure" % o
        <*< configPublicKey ..: "publicKey" % o
        <*< configThreadCount ..: "threadCount" % o

parseConfig :: MParser Config
parseConfig = id
    <$< configHashRate .:: option auto
        % short 'r'
        <> long "hash-rate"
        <> help "hashes per second"
    <*< configNode .:: option (textReader hostAddressFromText)
        % short 'n'
        <> long "node"
        <> help "node to which to connect"
        <> metavar "DOMAIN:PORT"
    <*< configUseTls .:: boolOption_
        % short 't'
        <> long "tls"
        <> help "use TLS to connect to node"
    <*< configInsecure .:: boolOption_
        % short 'x'
        <> long "insecure"
        <> help "accept self-signed TLS certificates"
    <*< configPublicKey .:: fmap MinerPublicKey . strOption
        % short 'k'
        <> long "public-key"
        <> help "the public-key for the mining rewards account"
    <*< configThreadCount .:: option auto
        % short 'c'
        <> long "thread-count"
        <> help "number of concurrent mining threads"

-- -------------------------------------------------------------------------- --
-- Chainweb Mining API Types

-- | Hash target. A little endian encoded 256 bit (unsigned) word.
--
newtype Target = Target BS.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

decodeTarget :: MonadGet m => m Target
decodeTarget = Target . BS.toShort <$> getBytes 32

encodeTarget :: MonadPut m => Target -> m ()
encodeTarget (Target b) = putByteString $ BS.fromShort b

-- | ChainId
--
newtype ChainId = ChainId Word32
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

decodeChainId :: MonadGet m => m ChainId
decodeChainId = ChainId <$> getWord32le

encodeChainId :: MonadPut m => ChainId -> m ()
encodeChainId (ChainId w32) = putWord32le w32

-- | Work bytes. The last 8 bytes are the nonce that is updated by the miner
-- while solving the work.
--
newtype Work = Work BS.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)

decodeWork :: MonadGet m => m (ChainId, Target, Work)
decodeWork = (,,)
    <$> decodeChainId
    <*> decodeTarget
    <*> do
        l <- fromIntegral <$> remaining
        Work . BS.toShort <$> getByteString l

-- -------------------------------------------------------------------------- --
-- Chainweb Mining API Requetss

newtype GetWorkFailure = GetWorkFailure T.Text
    deriving (Show, Eq, Ord)

instance Exception GetWorkFailure

-- | Make an HTTP request with an JSON response
--
getJson :: FromJSON a => HTTP.Manager -> HTTP.Request -> IO a
getJson mgr req = (eitherDecode . HTTP.responseBody <$> HTTP.httpLbs req mgr) >>= \case
    Left e -> error $ "Failed to decode json response: " <> show e
    Right r -> return r

-- | Base request type for chainweb queries
--
baseReq :: Config -> ChainwebVersion -> B.ByteString -> HTTP.Request
baseReq conf (ChainwebVersion v) pathSuffix = HTTP.defaultRequest
        { HTTP.host = T.encodeUtf8 $ hostnameToText $ _hostAddressHost node
        , HTTP.path = "chainweb/0.0/" <> T.encodeUtf8 v <> "/" <> pathSuffix
        , HTTP.port = fromIntegral $ _hostAddressPort node
        , HTTP.secure = _configUseTls conf
        , HTTP.method = "GET"
        , HTTP.responseTimeout = HTTP.responseTimeoutNone
        , HTTP.checkResponse = HTTP.throwErrorStatusCodes
        }
  where
    node = _configNode conf

-- | Query node info
--
getInfo :: Config -> HTTP.Manager -> IO (HM.HashMap T.Text Value)
getInfo conf mgr = getJson mgr req
  where
    req = HTTP.defaultRequest
        { HTTP.host = T.encodeUtf8 $ hostnameToText $ _hostAddressHost node
        , HTTP.path = "info"
        , HTTP.port = fromIntegral $ _hostAddressPort node
        , HTTP.secure = _configUseTls conf
        , HTTP.method = "GET"
        , HTTP.responseTimeout = HTTP.responseTimeoutNone
        , HTTP.checkResponse = HTTP.throwErrorStatusCodes
        }
    node = _configNode conf

-- | Obtain chainweb version of the chainweb node
--
getNodeVersion :: Config -> HTTP.Manager -> IO ChainwebVersion
getNodeVersion conf mgr = do
    i <- getInfo conf mgr
    case HM.lookup "nodeVersion" i of
        Just (String x) -> return $ ChainwebVersion x
        _ -> error "failed to parse chainweb version from node info"

-- | Get new work from the chainweb node (for some available chain)
--
getWork :: Config -> ChainwebVersion -> HTTP.Manager -> IO (ChainId, Target, Work)
getWork conf ver mgr = do
    bytes <- HTTP.httpLbs req mgr
    case runGetS decodeWork (BL.toStrict $ HTTP.responseBody $ bytes) of
        Left e -> error $ "failed to decode work: " <> sshow e
        Right (a,b,c) -> return (a, b, c)
  where
    req = (baseReq conf ver "mining/work")
        { HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ Miner $ _configPublicKey conf
        , HTTP.requestHeaders = [("content-type", "application/json")]
        }

-- | Post solved work to the chainweb node
--
postSolved :: Config -> ChainwebVersion -> HTTP.Manager -> Work -> IO ()
postSolved conf ver mgr (Work bytes) = do
    logInfo "post solved worked"
    (void $ HTTP.httpLbs req mgr)
        `catch` \(e@(HTTP.HttpExceptionRequest _ _)) -> do
            logError $ "failed to submit solved work: " <> sshow e
            return ()
  where
    req = (baseReq conf ver "mining/solved")
        { HTTP.requestBody = HTTP.RequestBodyBS $ BS.fromShort $ bytes
        , HTTP.method = "POST"
        }

-- | Automatically restarts the stream when the response status is 2** and throws
-- and exception otherwise.
--
updateStream
    :: Config
    -> ChainwebVersion
    -> HTTP.Manager
    -> ChainId
    -> TVar Int
    -> IO ()
updateStream conf v mgr cid var =
    liftIO $ withEvents req mgr $ \updates -> updates
        & SP.filter realEvent
        & SP.chain (\_ -> logInfo $ "got update on chain " <> sshow cid)
        & SP.mapM_ (\_ -> atomically $ modifyTVar' var (+ 1))
  where
    realEvent ServerEvent{} = True
    realEvent _ = False

    req = (baseReq conf v "mining/updates")
        { HTTP.requestBody = HTTP.RequestBodyBS $ runPutS $ encodeChainId cid
        }

-- -------------------------------------------------------------------------- --
-- Trigger Type

data Reason = Timeout | Update | StreamClosed | StreamFailed SomeException
    deriving (Show)

-- | A trigger is used to preempt a worker thread.
--
newtype Trigger = Trigger (STM Reason)

awaitTrigger :: Trigger -> IO Reason
awaitTrigger (Trigger t) = atomically t

-- -------------------------------------------------------------------------- --
-- Update Map

newtype UpdateFailure = UpdateFailure T.Text
    deriving (Show, Eq, Ord)

instance Exception UpdateFailure

-- | This keeps track of the current work for the respective chain. It is shared
-- among all worker threads. If an update for some chain is received the worker
-- threads are preempted and query new work.
--
-- It also keeps track of the long-polling update streams.
--
newtype UpdateMap = UpdateMap
    { _updateMap :: MVar (HM.HashMap ChainId (TVar Int, Async ()))
    }

-- | Creates a map that maintains one upstream for each chain
--
newUpdateMap :: IO UpdateMap
newUpdateMap = UpdateMap <$> newMVar mempty

-- | Obtain a trigger that is used to preempt a worker threads. It notifies the
-- thread if an update is available.
--
getTrigger
    :: Config
    -> ChainwebVersion
    -> HTTP.Manager
    -> UpdateMap
    -> ChainId
    -> IO Trigger
getTrigger conf ver mgr (UpdateMap v) k = modifyMVar v $ \m -> case HM.lookup k m of

    -- If there exists already an update stream, check that it's live, and
    -- restart if necessary.
    --
    Just s -> do
        logDebug "use existing update stream"
        n@(!var, !a) <- checkStream s
        !t <- newTrigger var a
        let !x = HM.insert k n m
        return (x, t)

    -- If there isn't an update stream in the map, create a new one.
    --
    Nothing -> do
        logDebug "create new update stream"
        n@(!var, !a) <- newTVarIO 0 >>= newUpdateStream
        !t <- newTrigger var a
        let !x = HM.insert k n m
        return (x, t)
  where
    checkStream :: (TVar Int, Async ()) -> IO (TVar Int, Async ())
    checkStream (!var, !a) = poll a >>= \case
        Nothing -> return (var, a)
        Just (Left _) -> newUpdateStream var -- TODO logging, throttling
        Just (Right _) -> newUpdateStream var

    newUpdateStream :: TVar Int -> IO (TVar Int, Async ())
    newUpdateStream var = (var,)
        <$> async (updateStream conf ver mgr k var)

    -- There are three possible outcomes
    --
    newTrigger :: TVar Int -> Async () -> IO Trigger
    newTrigger var a = do
        cur <- readTVarIO var
        timeoutVar <- registerDelay (5 * 30_000_000)
            -- 5 times the block time ~ 0.7% of all blocks. This for detecting if
            -- a stream gets stale without failing.

        return $ Trigger $ pollSTM a >>= \case
            Just (Right ()) -> return StreamClosed
            Just (Left e) -> return $ StreamFailed e
            Nothing -> do
                isTimeout <- readTVar timeoutVar
                isUpdate <- (/= cur) <$> readTVar var
                unless (isTimeout || isUpdate) retry
                return Update

-- | Run an operation that is preempted if an update event occurs.
--
-- Streams are restarted automatically, when they got closed by the server. We
-- don't restart streams automatically in case of a failure, but instead throw
-- an exception. Failures are supposed to be handled in the outer mining
-- functions.
--
-- There is risk that a stream stalls without explicitely failing. We solve this
-- by preempting the loop if we haven't seen an update after 5 times the block
-- time (which will affect about 0.7% of all blocks).
--
withPreemption
    :: Config
    -> ChainwebVersion
    -> HTTP.Manager
    -> UpdateMap
    -> ChainId
    -> IO a
    -> IO (Either () a)
withPreemption conf ver mgr m k = race awaitChange
  where
    awaitChange = do
        trigger <- getTrigger conf ver mgr m k
        awaitTrigger trigger >>= \case
            StreamClosed -> awaitChange
            StreamFailed e -> throwM $ UpdateFailure $ "update stream failed: " <> errMsg e
            Timeout -> throwM $ UpdateFailure "timeout of update stream"
            Update -> return ()

    errMsg e = case fromException e of
        Just (HTTP.HttpExceptionRequest _ ex) -> sshow ex
        _ -> sshow e

-- -------------------------------------------------------------------------- --
-- Mining Loop

data Recovery = Irrecoverable | Recoverable

-- | The outer mining loop.
--
miningLoop
    :: Config
    -> ChainwebVersion
    -> HTTP.Manager
    -> UpdateMap
    -> (Target -> Work -> IO Work)
    -> IO ()
miningLoop conf ver mgr umap inner = go
  where
    go = (forever loopBody `catches` handlers) >>= \case
        Irrecoverable -> return ()
        Recoverable -> threadDelay 500_000 >> go

    handlers =
        [ Handler $ \(e :: IOException) -> do
            logError $ T.pack $ displayException e
            return Irrecoverable
        , Handler $ \(e :: UpdateFailure) -> do
            logError $ T.pack $ displayException e
            return Irrecoverable
        , Handler $ \(e :: GetWorkFailure) -> do
            logError $ T.pack $ displayException e
            return Irrecoverable
        , Handler $ \(e :: SomeAsyncException) -> do
            logWarn $ "Mining Loop terminated: " <> sshow e
            throwM e
        , Handler $ \(e :: SomeException) -> do
            logWarn "Some general error in mining loop. Trying again..."
            logDebug $ T.pack $ displayException e
            return Recoverable
        ]

    loopBody = do
        (cid, target, work) <- getWork conf ver mgr
        logInfo $ "got new work for chain " <> sshow cid
        withPreemption conf ver mgr umap cid (inner target work) >>= \case
            Right solved -> do
                postSolved conf ver mgr solved
                logInfo "submitted work"
            Left () ->
                logInfo "Mining loop was preempted. Getting updated work ..."

-- -------------------------------------------------------------------------- --
-- Fake Mining Worker

-- | A fake mining worker that is not actually doing any work. It calculates the
-- solve time base on the assumed hash power of the worker thread and returns
-- the work bytes unchanged after that time has passed.
--
fakeWorker :: MWC.GenIO -> HashRate -> Target -> Work -> IO Work
fakeWorker rng rate (Target targetBytes) work = do
    delay <- round <$> MWC.exponential scale rng
    logInfo $ "solve time (microseconds): " <> sshow delay
    threadDelay delay
    return work
  where
    -- expectedMicros = 1_000_000 * difficulty / rate

    -- MWC.exponential is parameterized by the rate, i.e. 1 / expected_time
    scale = realToFrac $ realToFrac rate / (difficulty * 1_000_000)

    -- the expected number of attempts for solving a target is the difficulty.
    difficulty :: Rational
    difficulty = 2^(256 :: Integer) / targetNum

    -- Target is an little endian encoded (unsigned) 256 bit word.
    targetNum :: Rational
    targetNum = foldr (\b a -> fromIntegral b + 256 * a) 0 $ BS.unpack $ targetBytes

-- -------------------------------------------------------------------------- --
-- Main

mainInfo :: ProgramInfo Config
mainInfo = programInfo "Chainweb Test Miner for simulating POW" parseConfig defaultConfig

-- | TODO: validate the configuration:
--
-- * MinerPublicKey must be present
-- * node must be present
--
main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf ->
    run conf

run :: Config -> IO ()
run conf = do
    mgr <- HTTP.newManager $ HTTP.mkManagerSettings tlsSettings Nothing
    ver <- getNodeVersion conf mgr
    rng <- MWC.createSystemRandom
    updateMap <- newUpdateMap
    replicateConcurrently_ (fromIntegral $ _configThreadCount conf) $
        miningLoop conf ver mgr updateMap $ fakeWorker rng workerRate
  where
    tlsSettings = HTTP.TLSSettingsSimple (_configInsecure conf) False False

    workerRate = _configHashRate conf / fromIntegral (_configThreadCount conf)

