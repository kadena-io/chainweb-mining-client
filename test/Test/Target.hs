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

import qualified Data.Aeson as A
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Either
import Data.Function

import Test.QuickCheck
import Test.Syd

import Text.Read

-- internal modules

import Target

-- -------------------------------------------------------------------------- --
-- Generic Utilities

prop_show_read :: Eq a => Show a => Read a => Arbitrary a => a -> Property
prop_show_read a = read (show a) === a

prop_json_encode_decode
    :: Eq a
    => Show a
    => Arbitrary a
    => A.ToJSON a
    => A.FromJSON a
    => a
    -> Property
prop_json_encode_decode a = A.decode (A.encode a) === Just a

-- -------------------------------------------------------------------------- --
-- Utils

instance Arbitrary Target where
    arbitrary = Target . BS.pack <$> vector 32

newtype TargetBit = TargetBit Int
    deriving (Show, Eq, Ord)

instance Arbitrary TargetBit where
    arbitrary = TargetBit <$> chooseInt (0, 255)

slowTargetSet :: Int -> Target
slowTargetSet n = Target . BS.pack . reverse
    $ replicate x 0x00
    <> [shiftR 0xff y]
    <> replicate (31 - x) 0xff
  where
    (x, y) = n `quotRem` 8

slowTargetClz :: Target -> Int
slowTargetClz = B.foldr f 0 . B.reverse . BS.fromShort . _targetBytes
  where
    f (countLeadingZeros -> 8) r = 8 + r
    f (countLeadingZeros -> x) _ = x

-- -------------------------------------------------------------------------- --
-- Tests

tests :: Spec
tests = describe "Target tests" $ do
    prop "prop_targetCompLe" prop_targetCompLe
    prop "prop_targetClz" prop_targetClz
    prop "prop_targetSet" prop_targetSet
    prop "prop_targetSet_size" prop_targetSet_size
    prop "prop_targetSet_targetClz" prop_targetSet_targetClz
    prop "prop_json_encode_decode @Target" (prop_json_encode_decode @Target)
    prop "prop_show_read @Target" (prop_show_read @Target)

    prop_read_empty
    prop_read_wrong_size
    prop_read_size

    testTests

testTests :: Spec
testTests = describe "Target Test tests" $ do
    prop "prop_slowTargetSet_size" prop_slowTargetSet_size
    prop "prop_slowTargetSet_slowTargetClz" prop_slowTargetSet_slowTargetClz

-- -------------------------------------------------------------------------- --
-- Properties

prop_targetCompLe :: Target -> Target -> Property
prop_targetCompLe t0 t1 = targetCompLe t0 t1 === slowComp t0 t1
  where
    slowComp = compare `on` (B.reverse . BS.fromShort . _targetBytes)

prop_targetClz :: Target -> Property
prop_targetClz t = targetClz t === slowTargetClz t

prop_targetSet_size :: TargetBit -> Property
prop_targetSet_size (TargetBit b) = BS.length bytes === 32
  where
    Target bytes = targetSet (b + 1)

prop_targetSet :: TargetBit -> Property
prop_targetSet (TargetBit b) = targetSet (b + 1) === slowTargetSet (b + 1)

prop_targetSet_targetClz :: TargetBit -> Property
prop_targetSet_targetClz (TargetBit b) = targetClz (targetSet (b + 1)) === b + 1

prop_read_empty :: Spec
prop_read_empty = describe "target from empty string fails" $
    prop "read empty string fails" $ isLeft $ readEither @Target $ targetString ""

prop_read_wrong_size :: Spec
prop_read_wrong_size = describe "read target of wrong size fails" $ do
    flip mapM_ ([0..31] <> [33..40]) $ \s ->
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

-- -------------------------------------------------------------------------- --
-- Test Properties

prop_slowTargetSet_size :: TargetBit -> Property
prop_slowTargetSet_size (TargetBit b) = BS.length bytes === 32
  where
    Target bytes = slowTargetSet (b + 1)

prop_slowTargetSet_slowTargetClz :: TargetBit -> Property
prop_slowTargetSet_slowTargetClz (TargetBit b) = slowTargetClz (slowTargetSet (b + 1)) === b + 1

