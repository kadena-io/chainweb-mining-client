{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Logger
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A Lightweight Logging System
--
module Logger
( maxLoggerQueueSize
, Logger
, withLogger
, withLogTag
, writeLog
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad

import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System
import Data.Time.Format.ISO8601

import System.LogLevel

-- -------------------------------------------------------------------------- --
-- Constants

maxLoggerQueueSize :: Int
maxLoggerQueueSize = 10000

-- -------------------------------------------------------------------------- --
-- Utils

incrementCounter :: IORef Int -> IO ()
incrementCounter ref = atomicModifyIORef' ref $ \x -> (x + 1, ())

decrementCounter :: IORef Int -> IO ()
decrementCounter ref = atomicModifyIORef' ref $ \x -> (x - 1, ())

resetCounter :: IORef Int -> IO Int
resetCounter ref = atomicModifyIORef' ref $ \x -> (0, x)

-- -------------------------------------------------------------------------- --
--  Log Messages

-- | Log Message that is emitted by the code and enqueued in the logging queue.
--
-- The goal is to produce messages with very low latency in order to not delay
-- production logic. The most expensive part obtaining the system time, which is
-- stored raw without formatting.
--
-- When the LogMessage is written to the queue it must be fully evaluted to
-- normal form.
--
data LogMessage = LogMessage
    { _logMessageText :: !T.Text
    , _logMessageLevel :: !LogLevel
    , _logMessageTime :: !SystemTime
    , _logMessageTags :: ![T.Text]
    }

-- | Format Log message.
--
-- This is done in the logging backend asynchronously.
--
-- TODO: implement Chunk formatting.
--
formatLogMessage :: LogMessage -> T.Text
formatLogMessage !msg =
    (padTime . T.pack . iso8601Show . systemToUTCTime $ _logMessageTime msg)
    <> " "
    <> bracketed (logLevelToText (_logMessageLevel msg))
    <> bracketed (formatTags (_logMessageTags msg))
    <> " "
    <> _logMessageText msg
  where
    formatTags tags = T.intercalate "|" $ reverse tags
    bracketed t = "[" <> t <> "]"

    padTime t
        | T.length t == 28 = t
        | otherwise = T.take 27 (T.drop 1 t <> "000000") <> "Z"

-- -------------------------------------------------------------------------- --
-- Logger Context

data Logger = Logger
    { _loggerTags :: [T.Text]
    , _loggerLevel :: !LogLevel
    , _loggerQueue :: !(Chan LogMessage)
    , _loggerApproxQueueSize :: !(IORef Int)
    , _loggerSkipped :: !(IORef Int)
    , _loggerMaxQueueSize :: !Int
    }

-- | Locally push a tag to the stack of log message tags.
--
withLogTag :: Logger -> T.Text -> (Logger -> IO a) -> IO a
withLogTag !logger !tag inner = inner $! logger
    { _loggerTags = tag : _loggerTags logger
    }

-- | Write a log message.
--
-- A common pattern is to define a local helper function:
--
-- @
-- where
--   logg = writeLog logger
-- @
--
writeLog :: Logger -> LogLevel -> T.Text -> IO ()
writeLog !logger !level !msg
    | level <= _loggerLevel logger = mask_ $ do
        -- Nothing in here is expected to block or throw. So the mask should be
        -- sufficient. (TODO what about the getSystemTime?)
        c <- readIORef (_loggerApproxQueueSize logger)
        if (c > _loggerMaxQueueSize logger)
          then incrementCounter (_loggerSkipped logger)
          else do
            skipped <- resetCounter (_loggerSkipped logger)
            now <- getSystemTime
            when (skipped > 0) $ do
                writeChan (_loggerQueue logger) $ skippedMessage now skipped
                incrementCounter (_loggerApproxQueueSize logger)
            writeChan (_loggerQueue logger) $ LogMessage
                { _logMessageText = msg
                , _logMessageLevel = level
                , _logMessageTime = now
                , _logMessageTags = _loggerTags logger
                }
            incrementCounter (_loggerApproxQueueSize logger)

    | otherwise = return ()
  where
    skippedMessage now skipped = LogMessage
        { _logMessageText = "Skipped " <> T.pack (show skipped) <> " log messages."
        , _logMessageLevel = Error
        , _logMessageTime = now
        , _logMessageTags = _loggerTags logger
        }

-- -------------------------------------------------------------------------- --
-- Logging Backend Worker

-- | Initialize a logger context and start a background worker for writing
-- log messages to stdout.
--
withLogger :: LogLevel -> (Logger -> IO a) -> IO a
withLogger !level inner = do
    queue <- newChan
    sizeRef <- newIORef 0
    skippedRef <- newIORef 0
    r <- race (backend queue sizeRef) $ inner $ Logger
        { _loggerQueue = queue
        , _loggerLevel = level
        , _loggerTags = []
        , _loggerApproxQueueSize = sizeRef
        , _loggerSkipped = skippedRef
        , _loggerMaxQueueSize = maxLoggerQueueSize
        }
    case r of
        Left () -> error "logger existed unexpectedly"
        Right a -> return a
  where
    -- TODO: implement batch processing
    --
    backend queue sizeRef = forever $ do
        -- No need for masking here. If anything throws in here we exit the
        -- application anyways.
        --
        -- TODO: there is no way to check if Chan is empty. Use a different
        -- queue that allows to check that sizeRef and the Queue are
        -- approximately in sync.
        --
        msg <- readChan queue
        decrementCounter sizeRef
        T.putStrLn $ formatLogMessage msg

