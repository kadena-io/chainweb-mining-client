{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Worker.Stratum.Server
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Worker.Stratum.Server
( withStratumServer
, Job(..)
, StratumServerCtx(..) -- TODO only expose API
, StratumDifficulty(..)
, stratumDifficultyFromText
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
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe
import Data.Streaming.Network
import qualified Data.Text as T

import Network.HostAddress

import qualified Streaming.Prelude as S

import System.Clock
import System.IO.Unsafe
import qualified System.LogLevel as L
import System.Random.MWC

import Text.Read

-- internal modules

import JsonRpc
import Logger
import Target
import Utils
import Worker
import WorkerUtils
import Worker.Stratum.Protocol

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
-- Intialize Global Stratum Server

serverSalt :: Int
serverSalt = unsafePerformIO $ do
    createSystemRandom >>= uniform
{-# NOINLINE serverSalt #-}

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
noopJob = unsafePerformIO $ Job noJobId nullTarget (Work "") <$> newEmptyMVar
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

    , _ctxCurrentId :: !(IORef JobId)
        -- ^ Ticket counter for job ids.

    , _ctxDifficulty :: !StratumDifficulty
    }

newStratumServerCtx :: StratumDifficulty -> IO StratumServerCtx
newStratumServerCtx spec = StratumServerCtx
    (\_ _ -> return (Right ()))
    <$> newTVarIO mempty
    <*> newTVarIO noopJob
    <*> newIORef noJobId
    <*> pure spec

-- -------------------------------------------------------------------------- --
-- Sessions

defaultNonce1Size :: NonceSize
defaultNonce1Size = fromJust $ nonceSize @Int 2

sessionNonce2Size :: SessionState -> NonceSize
sessionNonce2Size = complementNonceSize . nonce1Size . _sessionNonce1

data SessionState = SessionState
    { _sessionNonce1 :: !Nonce1
        -- ^ a fixed unique nonce that is derived from the application
        -- and a server salt. It is used as prefix to provide the session
        -- with a private space of nonces.
    , _sessionTarget :: !(TVar Target)
    , _authorized :: !(TMVar Username)
    , _subscribed :: !(TMVar Agent)

    -- the followint entries are used to compute the estimated hashrate
    , _sessionLastShare :: !(TVar TimeSpec)
    , _sessionHashRateSum :: !(TVar HashRate)
    , _sessionShardCount :: !(TVar Int)
        -- ^ Estiumated hash rate of current session. Meassured as Hashes per second
    }

estimateHashRate :: SessionState -> IO HashRate
estimateHashRate ctx = do
    now <- getTime Monotonic

    atomically $ do
        lastShare <- swapTVar (_sessionLastShare  ctx) now
        Difficulty currentDifficulty <- targetToDifficulty <$> readTVar (_sessionTarget ctx)
        HashRate prevR <- readTVar (_sessionHashRateSum ctx)
        curCount <- (+ 1) <$> readTVar (_sessionShardCount ctx)

        let currentPeriodSeconds = realToFrac (toNanoSecs (diffTimeSpec now lastShare)) / 1_000_000_000
        let recentHashRate = currentDifficulty / currentPeriodSeconds
        let newAvgHashRateSum = prevR + recentHashRate

        writeTVar (_sessionHashRateSum ctx) $ HashRate newAvgHashRateSum
        writeTVar (_sessionShardCount ctx) curCount

        return $ HashRate $ newAvgHashRateSum / int curCount

data AppResult
    = JsonRpcError T.Text
    | StratumError T.Text
    | TimeoutError T.Text
    | ConnectionClosed T.Text
    deriving (Show, Eq, Ord)

instance Exception AppResult

type RequestStream = S.Stream (S.Of MiningRequest) IO AppResult

send :: A.ToJSON a => AppData -> a -> IO ()
send app a = appWrite app (BL.toStrict $ A.encode a) >> appWrite app "\n"

replyError :: AppData -> Error -> IO ()
replyError = send

reply :: AppData -> MiningResponse -> IO ()
reply = send

targetPeriod :: Period
targetPeriod = Period 10

notify :: Logger -> AppData -> SessionState -> Job -> IO ()
notify logger app sessionCtx job = do
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
    n2s = sessionNonce2Size sessionCtx

    go :: B.ByteString -> S.Stream (S.Of MiningRequest) IO AppResult
    go i = P.parseWith (liftIO (appRead app)) A.json' i >>= \case
        P.Fail _ path err ->
            return $ JsonRpcError $ "Failed to parse JSON RPC message: " <> T.pack err <> " " <> sshow path
        P.Partial _cont ->
            -- Can this actually happen, or would it a P.Fail?
            return $ ConnectionClosed "Connection closed unexpectedly"
        P.Done i' val -> case A.parse (parseMiningRequest n2s) val of
            A.Error err ->
                return $ StratumError $ "Unrecognized message: " <> T.pack err <> ". " <> sshow val
            A.Success result -> do
                S.yield result
                go i'

-- -------------------------------------------------------------------------- --
-- Stratum Server Config

data StratumDifficulty
  = WorkDifficulty
  | DifficultyLevel Level
  | DifficultyPeriod Period
  deriving (Show, Eq, Ord)

stratumDifficultyFromText :: MonadThrow m => T.Text -> m StratumDifficulty
stratumDifficultyFromText "block" = return WorkDifficulty
stratumDifficultyFromText t = case readEither @Int $ T.unpack t of
  Right n
    | 0 <= n && n <= 256 -> return $ DifficultyLevel $ level n
    | otherwise -> throwM $ FromTextException $ "Illegal value for stratum difficulty level. Expected a number between 0 and 256 but got " <> sshow n
  Left _ ->
    throwM $ FromTextException $ "failed to read stratum difficulty specification: " <> t

instance A.ToJSON StratumDifficulty where
  toJSON WorkDifficulty = "block"
  toJSON (DifficultyLevel i) = A.toJSON i
  toJSON (DifficultyPeriod i) = error "ToJSON StratumDifficulty: difficulty period is currently not supported"

instance A.FromJSON StratumDifficulty where
  parseJSON v = case v of
    A.String "block" -> return WorkDifficulty
    i@A.Number{} -> DifficultyLevel <$> A.parseJSON i
    e -> fail $ "failed to parse stratum difficulty specification: " <> show e

-- -------------------------------------------------------------------------- --
-- Target Adjustment

-- | ASICs are powerful machines. When connected to a network with low
-- difficulty it may produce a large amount of shares that can overload the
-- network, the stratum server, or the device itself. For that reason we set a
-- minimum target -- even if it means that the client does more work than needed
-- to solve a single block. In the end, producing blocks faster than they can be
-- processed down stream has no benefit and can even be harmful.
--
-- TODO: this should be configurable (possibly also on a per agent base)
--
maxSessionTarget :: Target
maxSessionTarget = mkTargetLevel $ level (42 :: Int)

-- | Start easy, the target will adjust quickly
--
initialTarget :: Target
initialTarget = maxSessionTarget

periodTolerance :: Double
periodTolerance = 0.25

-- | Purely compute a new session target
--
getNewSessionTarget
    :: StratumDifficulty
        -- ^ type of target computation
    -> HashRate
        -- ^ Current estimated hash rate
    -> Target
        -- ^ Current session target
    -> Target
        -- ^ job target
    -> Maybe Target
        -- ^ updated target
getNewSessionTarget stratumDifficulty currentHashRate currentTarget jobTarget
    | newTarget == currentTarget = Nothing
    | otherwise = Just newTarget
  where
    newTarget = case stratumDifficulty of
        WorkDifficulty -> jobTarget
        DifficultyLevel l -> mkTargetLevel l
        DifficultyPeriod p -> newPeriodTarget p

    -- The final target must be inbetween maxSessionTarget and jobTarget
    newPeriodTarget p = max jobTarget (min maxSessionTarget candidate)
      where
        curD = targetToDifficulty currentTarget
        newD = adjustDifficulty periodTolerance currentHashRate targetPeriod curD
        candidate = leveled $ difficultyToTarget newD

-- | Update the session target when a new share is found.
--
updateSessionTarget
    :: Logger
    -> StratumServerCtx
    -> AppData
    -> SessionState
    -> Job
    -> IO ()
updateSessionTarget logger serverCtx app sessionCtx job = do

    newHashRate <- estimateHashRate sessionCtx

    -- there is the chance of some race here, if we get several shares
    -- concurrently. It's possible that the effective target doesn't match the
    -- latest update of the period. We anways consider all previous targets, So
    -- not sure how much we care about this race.

    -- FIXME Find out whether changing target slows down the ASICs
    --
    -- TODO: the pool context should provide guidance what session target
    -- should be used.
    --
    currentTarget <- readTVarIO (_sessionTarget sessionCtx)
    writeLog logger L.Info $ "calculating new target"
        <> "; hashrate: " <> sshow newHashRate
        <> "; " <> prettyTarget "session" currentTarget
        <> "; " <> prettyTarget "job" (_jobTarget job)
        <> "; target Priod: " <> sshow targetPeriod
    case getNewSessionTarget (_ctxDifficulty serverCtx) newHashRate currentTarget (_jobTarget job) of
        Just t -> do
            writeLog logger L.Info $ "setting session target"
                <> "; " <> prettyTarget "new" t
                <> "; " <> prettyTarget "old" currentTarget
                <> "; " <> prettyTarget "job" (_jobTarget job)
                <> "; hashrate: " <> sshow newHashRate
            send app $ SetTarget $ T1 t
            atomically $ writeTVar (_sessionTarget sessionCtx) t
                -- Note, that there is a small chance of a race here, if the device is really fast
                -- and returns a solution before this is updated. We could solve that by
                -- making the target a TMVar or MVar and taking it before we send the
                -- "mining.set_target".
                --
                -- Most likely target changes are minor and shares are accepted even in
                -- case of a race.

        Nothing -> do
            t <- readTVarIO $ _sessionTarget sessionCtx
            writeLog logger L.Info $ "session target unchanged"
                <> "; " <> prettyTarget "session" t
                <> "; " <> prettyTarget "job" (_jobTarget job)
  where
    prettyTarget :: T.Text -> Target -> T.Text
    prettyTarget l t = l <> " " <> sshow t <> "[" <> sshow (getTargetLevel t) <> "]"

-- -------------------------------------------------------------------------- --
-- Session Loop

initialSessionState :: AppData -> IO SessionState
initialSessionState app = do
    SessionState sessionNonce1
        <$> newTVarIO initialTarget
        <*> newEmptyTMVarIO
        <*> newEmptyTMVarIO
        <*> (getTime Monotonic >>= newTVarIO)
        <*> newTVarIO (HashRate 0)
        <*> newTVarIO 0
  where
    sessionNonce1 = deriveNonce1 defaultNonce1Size serverSalt
        $ show (appSockAddr app) <> show (appLocalAddr app)

session :: Logger -> StratumServerCtx -> AppData -> IO ()
session l ctx app = withLogTag l "Stratum Session" $ \l2 -> withLogTag l2 (sshow (appSockAddr app)) $ \logger ->
    flip finally (appCloseConnection app) $ do
        sessionCtx <- initialSessionState app
        writeLog logger L.Info $ "new session"
            <> "; sessionNonce1 " <> sshow (_sessionNonce1 sessionCtx)

        -- Run Request Stream and Job Stream
        r <- race (processRequests logger sessionCtx) $ do
            awaitSubscribe sessionCtx

            -- initial target
            now <- getTime Monotonic
            atomically $ writeTVar (_sessionLastShare sessionCtx) now
            curJob <- liftIO $ readTVarIO (_ctxCurrentJob ctx)
            let jt = _jobTarget curJob
            -- TODO not sure what is a good starting value...
            -- let t = max jt (min maxSessionTarget (avgTarget maxSessionTarget jt))
            let t = max jt maxSessionTarget
            writeLog logger L.Info $ "setting initial session target: " <> sshow t
            atomically $ writeTVar (_sessionTarget sessionCtx) t
            send app $ SetTarget $ T1 t

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
            reply app $ subscribeResponse mid (_sessionNonce1 sessionCtx) (sessionNonce2Size sessionCtx)
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

                    -- we make sure that we never reject shares that solved a block!
                    -- (this add a little bit of skew to the share computation but everybody
                    -- benefits from it. The actual problem is that we don't associate
                    -- shares-works with their respective targets at notification in first place.
                    -- Also, this mostly affects solo miners that use sessions targets that
                    -- are, or a close to, the job targets.)
                    st <- max (_jobTarget job) <$> readTVarIO (_sessionTarget sessionCtx)
                    checkTarget st finalWork >>= \case

                        -- Invalid Share:
                        False -> do
                            writeLog jlog L.Warn "reject invalid nonce"
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

                            updateSessionTarget logger ctx app sessionCtx job

                            -- We've got a valid share
                            --
                            -- TODO: record share in the Pool Context
                            writeLog jlog L.Info $ "got valid share"
                                <> "; nonce2:" <> sshow n2
                                <> "; nonce: " <> sshow n
                                <> "; work: " <> sshow finalWork
                                <> "; target: " <> sshow (_jobTarget job)

                            -- Check whether it is a solution for the job and
                            -- only submit if it is. We do this here in order to
                            -- fail early and avoid contention on the job result
                            -- MVar.

                            -- TODO we could save a few CPU cycles by reusing the hash
                            -- from the previous check
                            checkTarget (_jobTarget job) finalWork >>= \case
                                False ->
                                    reply app $ SubmitResponse mid (Right True)
                                True -> do
                                    writeLog jlog L.Info $ "solved block: nonce2:" <> sshow n2 <> "; nonce: " <> sshow n
                                    -- Yeah, we've solved a block
                                    -- Commit final result to job
                                    void $ tryPutMVar (_jobResult job) n
                                    reply app $ SubmitResponse mid (Right True)

-- -------------------------------------------------------------------------- --
-- Stratum Server

withStratumServer
    :: Logger
    -> Port
    -> HostPreference
    -> StratumDifficulty
    -> (StratumServerCtx -> IO ())
    -> IO ()
withStratumServer l port host spec inner = withLogTag l "Stratum Server" $ \logger -> do
    ctx <- newStratumServerCtx spec
    race (server logger ctx) (inner ctx) >>= \case
        Left _ -> writeLog logger L.Error "server exited unexpectedly"
        Right _ -> do
            writeLog logger L.Error "mining loop existed unexpectedly"
            return ()
  where
    server logger ctx = flip finally (writeLog logger L.Info "server stopped") $ do
        writeLog logger L.Info "Start stratum server"
        runTCPServer (serverSettingsTCP (int port) host) (session logger ctx)

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


