{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Utils
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.Utils
( tests
) where

import Data.Word

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Numeric.Natural

import System.IO.Unsafe

import Test.QuickCheck
import Test.Syd

import Test.QuickCheck.Instances ({- Arbitrary Natural -})

-- internal modules

import Utils

-- -------------------------------------------------------------------------- --
-- Tests

tests :: Spec
tests = describe "Utils" $ do
    prop "prop_naturalLog2" prop_naturalLog2
    prop "prop_le64_poke" prop_le64_poke
    prop "prop_le64_peek" prop_le64_peek

word8LeToWord64 :: [Word8] -> Word64
word8LeToWord64 w8s = foldr (\a c -> (int @Word8 a) + c * 256) 0 w8s

-- -------------------------------------------------------------------------- --
-- Properties

prop_naturalLog2 :: Natural -> Property
prop_naturalLog2 x = naturalLog2 x === naturalLog2_compat x

prop_le64_poke :: Word64 -> Property
prop_le64_poke w = word8LeToWord64 w8s === w
  where
    w8s = unsafePerformIO $ with (le64 w) $ peekArray 8 . castPtr

prop_le64_peek :: Property
prop_le64_peek = property $ do
    w8s <- vector 8
    let w = unsafePerformIO $ withArray w8s $ peek @Word64 . castPtr
    return $ word8LeToWord64 w8s === w
