{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Test.WorkerUtils
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.WorkerUtils
( tests
) where

import qualified Data.Aeson as A
import Data.Bytes.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Maybe
import Data.Word

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import Test.Syd

-- internal modules

import Target

import Worker

import WorkerUtils

import Utils

-- -------------------------------------------------------------------------- --
-- Tests

tests :: Spec
tests = do
    test_checkTarget_testHeaders

-- -------------------------------------------------------------------------- --
-- Test headers

newtype Header = Header { _headerBytes :: BS.ShortByteString }

decodeHeader :: MonadGet m => m Header
decodeHeader = Header . BS.toShort <$> getBytes 318
{-# INLINE decodeHeader #-}

testHeaders :: [Header]
testHeaders = unsafePerformIO $ do
    bs <- B.readFile "test/data/test-headers.bin"
    case go bs of
        Left e -> error $ "parsing testHeaders failed: " <> e
        Right x -> return x
  where
    go (B.splitAt 318 -> (a,b)) = (:)
        <$> runGetS decodeHeader a
        <*> parse b
    parse b
        | B.null b = return mempty
        | B.length b < 318 = Left $ "missing input bytes. Expected 318; got " <> show (B.length b)
        | otherwise = go b
{-# NOINLINE testHeaders #-}

extractTarget :: Work -> Target
extractTarget (Work bytes) = targetFromWords $ unsafePerformIO $
    BS.useAsCStringLen bytes $ \(ptr,_) -> TargetWords
        <$> peekWord64OffLe (castPtr ptr) 158
        <*> peekWord64OffLe (castPtr ptr) 166
        <*> peekWord64OffLe (castPtr ptr) 174
        <*> peekWord64OffLe (castPtr ptr) 182

testWorks :: [Work]
testWorks = Work . BS.toShort . B.take 286 . BS.fromShort . _headerBytes <$> testHeaders

-- -------------------------------------------------------------------------- --
-- Test cases

test_checkTarget_testHeaders :: Spec
test_checkTarget_testHeaders = describe "WorkerUtils.checkTarget succeeds for test headers"
    $ mapM_ (uncurry checkWork) (zip [0..] testWorks)
  where
    checkWork i w = it (show i) $
        checkTarget (extractTarget w) w `shouldReturn` True

