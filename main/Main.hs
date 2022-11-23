{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
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
import Control.Retry

import Crypto.Hash.Algorithms (Blake2s_256)
import qualified Crypto.PubKey.Ed25519 as C

import qualified Data.ByteArray.Encoding as BA
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Streaming.Network
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

import GHC.Generics

import qualified Network.Connection as HTTP
import Network.HostAddress
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Network.Wai.EventSource.EventStream
import Network.Wai.EventSource.Streaming

import Numeric.Natural

import PkgInfo hiding (tag)

import qualified Streaming.Prelude as SP

import System.LogLevel
import qualified System.Random.MWC as MWC

import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as P
import Text.Printf
import Text.Read (Read(..), readListPrecDefault)

-- internal modules

import Utils
import Logger
import Worker
import Worker.CPU
import Worker.External
import Worker.Simulation
import qualified Worker.Stratum as Stratum
import qualified Worker.Stratum.Server as Stratum

-- -------------------------------------------------------------------------- --
-- Integral Unit Prefixes

-- | TODO: make this type roundtripable and add support for fractional unit
-- prefixes.
--
newtype UnitPrefixed a = UnitPrefixed { _getUnitPrefixed :: a }
    deriving newtype
        ( Show
        , Eq
        , Ord
        , Enum
        , Bounded
        , Num
        , Integral
        , Fractional
        , Floating
        , Real
        , ToJSON
        , FromJSON
        )

instance (Num a, Read a) => Read (UnitPrefixed a) where
    readPrec = UnitPrefixed <$> readWithUnit
    readListPrec = readListPrecDefault
    {-# INLINE readPrec #-}
    {-# INLINE readListPrec #-}

-- | Read number with Unit Prefixes. The implementation supports integral SI
-- units prefixes. Binary prefixes are supported according to ISO/IEC 80000.
--
-- "Kilo" is supported, both in upper and lower case.
--
readWithUnit :: forall a . Num a => Read a => P.ReadPrec a
readWithUnit = do
    n <- readPrec
    p <- P.lift $ noPrefix <|> siPrefix <|> binaryPrefix
    return $! n * p
  where
    noPrefix :: R.ReadP a
    noPrefix = 1 <$ R.eof

    siPrefix :: R.ReadP a
    siPrefix
        = 10^(1 :: Int) <$ R.string "da"
        <|> 10^(2 :: Int) <$ R.char 'h'
        <|> 10^(3 :: Int) <$ (R.char 'K' <|> R.char 'k')
        <|> 10^(6 :: Int) <$ R.char 'M'
        <|> 10^(9 :: Int) <$ R.char 'G'
        <|> 10^(12 :: Int) <$ R.char 'T'
        <|> 10^(15 :: Int) <$ R.char 'P'
        <|> 10^(18 :: Int) <$ R.char 'E'
        <|> 10^(21 :: Int) <$ R.char 'Z'
        <|> 10^(24 :: Int) <$ R.char 'Y'

    binaryPrefix :: R.ReadP a
    binaryPrefix
        = 1024^(1 :: Int) <$ (R.string "Ki" <|> R.string "ki")
        <|> 1024^(2 :: Int) <$ R.string "Mi"
        <|> 1024^(3 :: Int) <$ R.string "Gi"
        <|> 1024^(4 :: Int) <$ R.string "Ti"
        <|> 1024^(5 :: Int) <$ R.string "Pi"
        <|> 1024^(6 :: Int) <$ R.string "Ei"
        <|> 1024^(7 :: Int) <$ R.string "Zi"
        <|> 1024^(8 :: Int) <$ R.string "Yi"

-- -------------------------------------------------------------------------- --
-- Miner

-- | The account name is that same as the public key. Different account names
-- are not supported.
--
-- Only a single base64UrlWithoutPadding encoded key may be used and the keyset
-- is "keys-all".
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

newtype MinerAccount = MinerAccount T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable, ToJSON, FromJSON)

data Miner = Miner MinerPublicKey (Maybe MinerAccount)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

instance ToJSON Miner where
    toJSON (Miner (MinerPublicKey k) account) = object
        [ "account" .= fromMaybe (MinerAccount ("k:" <> k)) account
        , "public-keys" .= [ k ]
        , "predicate" .= ("keys-all" :: T.Text)
        ]

-- -------------------------------------------------------------------------- --
-- Worker Configuration

data WorkerConfig
    = CpuWorker
    | ExternalWorker
    | SimulationWorker
    | StratumWorker
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

instance ToJSON WorkerConfig where
    toJSON = toJSON . workerConfigToText
    {-# INLINE toJSON #-}

instance FromJSON WorkerConfig where
    parseJSON = withText "WorkerConfig" $
        either (fail . show) return . workerConfigFromText
    {-# INLINE parseJSON #-}

workerConfigToText :: WorkerConfig -> T.Text
workerConfigToText CpuWorker = "cpu"
workerConfigToText ExternalWorker = "external"
workerConfigToText SimulationWorker = "simulation"
workerConfigToText StratumWorker = "stratum"

workerConfigFromText :: MonadThrow m => T.Text -> m WorkerConfig
workerConfigFromText t = case T.toCaseFold t of
    "cpu" -> return CpuWorker
    "external" -> return ExternalWorker
    "simulation" -> return SimulationWorker
    "stratum" -> return StratumWorker
    _ -> throwM $ FromTextException $ "unknown worker configuraton: " <> t

-- -------------------------------------------------------------------------- --
-- Configuration

newtype ChainwebVersion = ChainwebVersion T.Text
    deriving (Show, Read, Eq, Ord, Generic)
    deriving newtype (Hashable, ToJSON, FromJSON)

data Config = Config
    { _configHashRate :: !(UnitPrefixed HashRate)
    , _configNode :: !HostAddress
    , _configUseTls :: !Bool
    , _configInsecure :: !Bool
    , _configPublicKey :: !MinerPublicKey
    , _configAccount :: !(Maybe MinerAccount)
    , _configThreadCount :: !Natural
    , _configGenerateKey :: !Bool
    , _configLogLevel :: !LogLevel
    , _configWorker :: !WorkerConfig
    , _configExternalWorkerCommand :: !String
    , _configStratumPort :: !Port
    , _configStratumInterface :: !HostPreference
    , _configStratumDifficulty :: !Stratum.StratumDifficulty
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
    { _configHashRate = UnitPrefixed defaultHashRate
    , _configNode = unsafeHostAddressFromText "localhost:1789"
    , _configUseTls = True
    , _configInsecure = True
    , _configPublicKey = MinerPublicKey ""
    , _configAccount = Nothing
    , _configThreadCount = 2
    , _configGenerateKey = False
    , _configLogLevel = Info
    , _configWorker = StratumWorker
    , _configExternalWorkerCommand = "echo 'no external worker command configured' && /bin/false"
    , _configStratumPort = 1917
    , _configStratumInterface = "*"
    , _configStratumDifficulty = Stratum.WorkDifficulty
    }

instance ToJSON Config where
    toJSON c = object
        [ "hashRate" .= _configHashRate c
        , "node" .= _configNode c
        , "useTls" .= _configUseTls c
        , "insecure" .= _configInsecure c
        , "publicKey" .= _configPublicKey c
        , "account" .= _configAccount c
        , "threadCount" .= _configThreadCount c
        , "generateKey" .= _configGenerateKey c
        , "logLevel" .= logLevelToText @T.Text (_configLogLevel c)
        , "worker" .= _configWorker c
        , "externalWorkerCommand" .= _configExternalWorkerCommand c
        , "stratumPort" .= _configStratumPort c
        , "stratumInterface" .= _configStratumInterface c
        , "stratumDifficulty" .= _configStratumDifficulty c
        ]

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< configHashRate ..: "hashRate" % o
        <*< configNode ..: "node" % o
        <*< configUseTls ..: "useTls" % o
        <*< configInsecure ..: "insecure" % o
        <*< configPublicKey ..: "publicKey" % o
        <*< configAccount ..: "account" % o
        <*< configThreadCount ..: "threadCount" % o
        <*< configGenerateKey ..: "generateKey" % o
        <*< setProperty configLogLevel "logLevel" parseLogLevel o
        <*< configWorker ..: "worker" % o
        <*< configExternalWorkerCommand ..: "externalWorkerCommand" % o
        <*< configStratumPort ..: "stratumPort" % o
        <*< configStratumInterface ..: "stratumInterface" % o
        <*< configStratumDifficulty ..: "stratumDifficulty" % o
      where
        parseLogLevel = withText "LogLevel" $ return . logLevelFromText

parseConfig :: MParser Config
parseConfig = id
    <$< configHashRate .:: option auto
        % short 'r'
        <> long "hash-rate"
        <> help "hashes per second (only relevant for mining simulation, ignored by the cpu worker)"
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
        <> help "public-key for the mining rewards account"
    <*< configAccount .:: fmap (Just . MinerAccount) . strOption
        % short 'a'
        <> long "account"
        <> help "account for the mining rewards (default: public-key prefixed with 'k:')"
    <*< configThreadCount .:: option auto
        % short 'c'
        <> long "thread-count"
        <> help "number of concurrent mining threads"
    <*< configGenerateKey .:: boolOption_
        % long "generate-key"
        <> help "Generate a new key pair and exit"
    <*< configLogLevel .:: option (textReader $ Right . logLevelFromText)
        % short 'l'
        <> long "log-level"
        <> help "Level at which log messages are written to the console"
        <> metavar "error|warn|info|debug"
    <*< configWorker .:: option (textReader workerConfigFromText)
        % short 'w'
        <> long "worker"
        <> help "The type of mining worker that is used"
        <> metavar "cpu|external|simulation|stratum"
    <*< configExternalWorkerCommand .:: option (textReader $ Right . T.unpack)
        % long "external-worker-cmd"
        <> help "command that is used to call an external worker. When the command is called the target value is added as last parameter to the command line."
    <*< configStratumPort .:: option jsonReader
      % long "stratum-port"
      <> help "the port on which the stratum server listens"
    <*< configStratumInterface .:: option jsonReader
      % long "stratum-interface"
      <> help "network interface that the stratum server binds to"
    <*< configStratumDifficulty .:: option (textReader Stratum.stratumDifficultyFromText)
      % long "stratum-difficulty"
      <> help "How the difficulty for stratum mining shares is choosen. Possible values are \"block\" for using the block target of the most most recent notification of new work, or number between 0 and 256 for specifiying a fixed difficulty as logarithm of base 2 (number of leading zeros)."

-- -------------------------------------------------------------------------- --
-- HTTP Retry Logic

-- | We don't limit retries. The maximum delay between retries is 5 seconds.
--
-- TODO: add configuration option for limitRetriesByCumulativeDelay
--
retryHttp :: Logger -> IO a -> IO a
retryHttp logger = recovering policy (httpRetryHandler logger) . const
  where
    policy = capDelay 5000000 $ fullJitterBackoff 100

httpRetryHandler :: Logger -> [RetryStatus -> Handler IO Bool]
httpRetryHandler logger = skipAsyncExceptions <>
    [ logRetries (return . httpRetries) f
    , logRetries (\(_ :: SomeException) -> return True) logRetry
    ]
  where
    logRetry True reason s = writeLog logger Warn
        $ "Http request failed: " <> sshow reason
        <> ". Retrying attempt " <> sshow (rsIterNumber s)
    logRetry False reason s = writeLog logger Warn
        $ "Http request finally failed after " <> sshow (rsIterNumber s)
        <> " retries: " <> sshow reason

    f True (HTTP.HttpExceptionRequest _req reason) s = logRetry True reason s
    f False (HTTP.HttpExceptionRequest _req reason) s = logRetry False reason s
    f _ e _ = throwM e


-- | HTTP Exceptions for which a retry may result in subsequent succes.
--
-- This retries rather aggressively on any server or network related failure
-- condition.
--
httpRetries :: HTTP.HttpException -> Bool
httpRetries (HTTP.HttpExceptionRequest _req reason) = case reason of
    HTTP.StatusCodeException resp _body
        | HTTP.statusIsServerError (HTTP.responseStatus resp) -> True
    HTTP.ResponseTimeout -> True
    HTTP.ConnectionTimeout -> True
    HTTP.ConnectionFailure _e -> True
    HTTP.InvalidStatusLine _bs -> True
    HTTP.InvalidHeader _bs -> True
    HTTP.InternalException _e -> True
    HTTP.ProxyConnectException _host _port status
        | HTTP.statusIsServerError status -> True
    HTTP.NoResponseDataReceived -> True
    HTTP.ResponseBodyTooShort _expected _actual -> True
    HTTP.InvalidChunkHeaders -> True
    HTTP.IncompleteHeaders -> True
    HTTP.HttpZlibException _e -> True
    HTTP.ConnectionClosed -> True
    _ -> False
httpRetries (HTTP.InvalidUrlException _url _reason) = False

-- -------------------------------------------------------------------------- --
-- Chainweb Mining API Types

-- | ChainId
--
newtype ChainId = ChainId Word32
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

decodeChainId :: MonadGet m => m ChainId
decodeChainId = ChainId <$> getWord32le

encodeChainId :: MonadPut m => ChainId -> m ()
encodeChainId (ChainId w32) = putWord32le w32

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
        , HTTP.checkResponse = HTTP.throwErrorStatusCodes
        }
    node = _configNode conf

-- | Obtain chainweb version of the chainweb node
--
-- No retry here. This is use at startup and we want to fail fast if the node
-- isn't available.
--
getNodeVersion :: Config -> HTTP.Manager -> IO ChainwebVersion
getNodeVersion conf mgr = do
    i <- getInfo conf mgr
    case HM.lookup "nodeVersion" i of
        Just (String x) -> return $ ChainwebVersion x
        _ -> error "failed to parse chainweb version from node info"

-- | Get new work from the chainweb node (for some available chain)
--
-- We don't retry here. If this fails, we loop around.
--
getJob :: Config -> ChainwebVersion -> HTTP.Manager -> IO (ChainId, Target, Work)
getJob conf ver mgr = do
    bytes <- HTTP.httpLbs req mgr
    case runGetS decodeJob (BL.toStrict $ HTTP.responseBody bytes) of
        Left e -> error $ "failed to decode work: " <> sshow e
        Right (a,b,c) -> return (a, b, c)
  where
    req = (baseReq conf ver "mining/work")
        { HTTP.requestBody = HTTP.RequestBodyLBS
            $ encode
            $ Miner
                (_configPublicKey conf)
                (_configAccount conf)
        , HTTP.requestHeaders = [("content-type", "application/json")]
        }

    decodeJob :: MonadGet m => m (ChainId, Target, Work)
    decodeJob = (,,)
        <$> decodeChainId
        <*> decodeTarget
        <*> decodeWork

-- | Post solved work to the chainweb node
--
-- No timeout is used and in case of failure we retry aggressively. If the
-- solvedwork becomes stale, the thread will be preempted and cancled.
--
postSolved :: Config -> ChainwebVersion -> Logger -> HTTP.Manager -> Work -> IO ()
postSolved conf ver logger mgr (Work bytes) = retryHttp logger $ do
    logg Info "post solved worked"
    (void $ HTTP.httpLbs req mgr)
        `catch` \(e@(HTTP.HttpExceptionRequest _ _)) -> do
            logg Error $ "failed to submit solved work: " <> sshow e
            return ()
  where
    logg = writeLog logger
    req = (baseReq conf ver "mining/solved")
        { HTTP.requestBody = HTTP.RequestBodyBS $ BS.fromShort bytes
        , HTTP.method = "POST"
        }

-- | Automatically restarts the stream when the response status is 2** and throws
-- and exception otherwise.
--
-- No retry is used. Retrying is handled by the outer logic.
--
updateStream
    :: Config
    -> ChainwebVersion
    -> Logger
    -> HTTP.Manager
    -> ChainId
    -> TVar Int
    -> IO ()
updateStream conf v logger mgr cid var =
    liftIO $ withEvents req mgr $ \updates -> updates
        & SP.filter realEvent
        & SP.chain (\_ -> logg Info $ "got update on chain " <> sshow cid)
        & SP.mapM_ (\_ -> atomically $ modifyTVar' var (+ 1))
  where
    logg = writeLog logger

    realEvent ServerEvent{} = True
    realEvent _ = False

    req = (baseReq conf v "mining/updates")
        { HTTP.requestBody = HTTP.RequestBodyBS $ runPutS $ encodeChainId cid
        , HTTP.responseTimeout = HTTP.responseTimeoutNone
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
    -> Logger
    -> HTTP.Manager
    -> UpdateMap
    -> ChainId
    -> IO Trigger
getTrigger conf ver logger mgr (UpdateMap v) k = modifyMVar v $ \m -> case HM.lookup k m of

    -- If there exists already an update stream, check that it's live, and
    -- restart if necessary.
    --
    Just s -> do
        logg Debug "use existing update stream"
        n@(!var, !a) <- checkStream s
        !t <- newTrigger var a
        let !x = HM.insert k n m
        return (x, t)

    -- If there isn't an update stream in the map, create a new one.
    --
    Nothing -> do
        logg Debug "create new update stream"
        n@(!var, !a) <- newTVarIO 0 >>= newUpdateStream
        !t <- newTrigger var a
        let !x = HM.insert k n m
        return (x, t)
  where
    logg = writeLog logger

    checkStream :: (TVar Int, Async ()) -> IO (TVar Int, Async ())
    checkStream (!var, !a) = poll a >>= \case
        Nothing -> return (var, a)
        Just (Left _) -> newUpdateStream var -- TODO logging, throttling
        Just (Right _) -> newUpdateStream var

    newUpdateStream :: TVar Int -> IO (TVar Int, Async ())
    newUpdateStream var = (var,)
        <$> async (updateStream conf ver logger mgr k var)

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
    -> Logger
    -> HTTP.Manager
    -> UpdateMap
    -> ChainId
    -> IO a
    -> IO (Either () a)
withPreemption conf ver logger mgr m k = race awaitChange
  where
    awaitChange = do
        trigger <- getTrigger conf ver logger mgr m k
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
    -> Logger
    -> HTTP.Manager
    -> UpdateMap
    -> Worker
    -> IO ()
miningLoop conf ver logger mgr umap worker = go
  where
    nonce = Nonce 0
    logg = writeLog logger
    go = (forever loopBody `catches` handlers) >>= \case
        Irrecoverable -> return ()
        Recoverable -> threadDelay 500_000 >> go

    handlers =
        [ Handler $ \(e :: IOException) -> do
            logg Error $ T.pack $ displayException e
            return Irrecoverable
        , Handler $ \(e :: GetWorkFailure) -> do
            logg Error $ T.pack $ displayException e
            return Irrecoverable -- FIXME we want proper retry logic for all of this!
        , Handler $ \(e :: UpdateFailure) -> do
            logg Error $ T.pack $ displayException e
            return Recoverable
        , Handler $ \(e :: SomeAsyncException) -> do
            logg Warn $ "Mining Loop terminated: " <> sshow e
            throwM e
        , Handler $ \(e :: SomeException) -> do
            logg Warn "Some general error in mining loop. Trying again..."
            logg Info $ "Exception: " <> T.pack (displayException e)
            return Recoverable
        ]

    loopBody = do
        (cid, target, work) <- getJob conf ver mgr
        logg Info $ "got new work for chain " <> sshow cid
        withPreemption conf ver logger mgr umap cid (worker nonce target work) >>= \case
            Right solved -> do
                -- TODO: we should do this asynchronously, however, preemption
                -- should still apply. So, ideally, we would kick of a new
                -- asynchronous loop interation while continuing this loop
                -- iteration here.
                postSolved conf ver logger mgr solved
                logg Debug "submitted work"
            Left () ->
                logg Info "Mining loop was preempted. Getting updated work ..."

-- -------------------------------------------------------------------------- --
-- Key generation

genKeys :: IO ()
genKeys = do
    sk <- C.generateSecretKey
    let !pk = C.toPublic sk
    printf "public:  %s\n" (B8.unpack $ BA.convertToBase BA.Base16 pk)
    printf "private: %s\n" (B8.unpack $ BA.convertToBase BA.Base16 sk)

-- -------------------------------------------------------------------------- --
-- Main

mainInfo :: ProgramInfo Config
mainInfo = programInfo "Kadena Chainweb Mining Client" parseConfig defaultConfig

-- | TODO: validate the configuration:
--
-- * MinerPublicKey must be present
-- * node must be present
--
main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf ->
    if _configGenerateKey conf
      then genKeys
      else withLogger (_configLogLevel conf) $ run conf

run :: Config -> Logger -> IO ()
run conf logger = do
    mgr <- HTTP.newManager (HTTP.mkManagerSettings tlsSettings Nothing)
        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 1000000 }
            -- We don't want to wait too long, because latencies matter in
            -- mining. NOTE, however, that for large blocks it can take a while
            -- to get new work. This can be an issue with public mining mode.
            -- For private mining that is done asynchronously. Public mining is
            -- considered deprecated.
    ver <- getNodeVersion conf mgr
    updateMap <- newUpdateMap
    withWorker $ \worker -> do
        forConcurrently_ [0 .. _configThreadCount conf - 1] $ \i ->
            withLogTag logger ("Thread " <> sshow i) $ \taggedLogger ->
                miningLoop conf ver taggedLogger mgr updateMap (worker taggedLogger)
  where
    tlsSettings = HTTP.TLSSettingsSimple (_configInsecure conf) False False

    workerRate = _getUnitPrefixed (_configHashRate conf) / fromIntegral (_configThreadCount conf)

    -- provide the inner computation with an initialized worker
    withWorker f = case _configWorker conf of
        SimulationWorker -> do
            rng <- MWC.createSystemRandom
            f $ \l -> simulationWorker l rng workerRate
        ExternalWorker -> f $ \l -> externalWorker l (_configExternalWorkerCommand conf)
        CpuWorker -> f $ cpuWorker @Blake2s_256
        StratumWorker -> Stratum.withStratumServer
          logger
          (_configStratumPort conf)
          (_configStratumInterface conf)
          (_configStratumDifficulty conf)
          (f . Stratum.submitWork)

