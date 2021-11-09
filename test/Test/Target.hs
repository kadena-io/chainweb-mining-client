{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Test.Target
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.Target
( tests
) where

import Control.Monad

import qualified Data.Aeson as A
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Char
import Data.Either
import Data.Function
import Data.String
import qualified Data.Text as T
import Data.Word

import Test.QuickCheck
import Test.Syd

import Text.Read

-- internal modules

import Target

import TestUtils

import Utils

-- -------------------------------------------------------------------------- --
-- Utils

instance Arbitrary Target where
    arbitrary = Target . fromIntegral <$> chooseInteger (0, 2^256-1)

newtype TargetBit = TargetBit Int
    deriving (Show, Eq, Ord)

instance Arbitrary TargetBit where
    arbitrary = TargetBit <$> chooseInt (0, 255)

instance Arbitrary Level where
    arbitrary = level <$> choose @Int (0, 255)

nibbleClz :: Char -> Int
nibbleClz = (\x -> x - 4) . countLeadingZeros . int @_ @Word8 . digitToInt

hexClz :: T.Text -> Int
hexClz hexStr = case T.uncons <$> T.break (/= '0') hexStr of
    (a, Nothing) -> T.length a * 4
    (a, Just (b, _)) -> T.length a * 4 + nibbleClz b

-- -------------------------------------------------------------------------- --
-- Tests

tests :: Spec
tests = describe "Target tests" $ do
    prop "prop_json_encode_decode @Target" (prop_json_encode_decode @Target)
    prop "prop_show_read @Target" (prop_show_read @Target)

    prop "prop_targetWords" prop_targetWords
    prop "prop_fromString" prop_fromString
    prop "prop_targetLevel" prop_targetLevel
    prop "prop_getTargetLevel" prop_targetLevel

    prop_read_empty
    prop_read_wrong_size
    prop_read_size

-- -------------------------------------------------------------------------- --
-- Properties

prop_read_empty :: Spec
prop_read_empty = describe "target from empty string fails" $
    prop "read empty string fails" $ isLeft $ readEither @Target $ targetString ""

prop_read_wrong_size :: Spec
prop_read_wrong_size = describe "read target of wrong size fails" $ do
    forM_ ([0..31] <> [33..40]) $ \s ->
        prop "read string of wrong size failes"
            $ fmap (isLeft . readEither @Target . targetString)
            $ vectorOf (s * 2)
            $ elements "0123456789abcdef"

prop_read_size :: Spec
prop_read_size = prop "read string of correct size succeeds"
    $ fmap (isRight . readEither @Target . targetString)
    $ vectorOf (32 * 2)
    $ elements "0123456789abcdef"

targetString :: String -> String
targetString s = "Target \"" <> s <> "\""

prop_targetWords :: Target -> Property
prop_targetWords t = targetFromWords (targetToWords t) === t

prop_fromString :: Target -> Property
prop_fromString t = (fromString $ T.unpack $ targetToText16Be t) === t

-- -------------------------------------------------------------------------- --
-- Level Stuff

prop_targetLevel :: Level -> Property
prop_targetLevel l =
    level lzc === l
    .&&.
    T.all (== 'f') r === True
  where
    (lzc, r) = case T.uncons <$> T.break (/= '0') str of
        (a, Nothing) -> (T.length a * 4, r)
        (a, Just (b, c)) -> (T.length a * 4 + nibbleClz b , c)

    str = targetToText16Be (mkTargetLevel l)

prop_getTargetLevel :: Target -> Property
prop_getTargetLevel t = getTargetLevel t === level (hexClz (targetToText16Be t))
