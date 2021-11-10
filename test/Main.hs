{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Test.QuickCheck

import Test.Syd

-- internal modules

import qualified Test.Target
import qualified Test.Utils
import qualified Test.WorkerUtils

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = sydTest tests

tests :: Spec
tests = do
    describe "Test.Target" Test.Target.tests
    describe "Test.Utils" Test.Utils.tests
    describe "Test.WorkerUtils" Test.WorkerUtils.tests

-- -- -------------------------------------------------------------------------- --
-- -- ByteSwap
--
-- prop_byteSwap_id :: ByteSwap a => a -> Property
-- prop_byteSwap_id a = a === (byteSwap . byteSwap) a
--
-- byteSwap_properties =
--     [ "@Int", property $ prop_byteSwap_id @Int
--     , "@Word", property $ prop_byteSwap_id @Word
--     , "@Word8", property $ prop_byteSwap_id @Word8j
--     , "@Word16", property $ prop_byteSwap_id @Word16
--     , "@Word32", property $ prop_byteSwap_id @Word32
--     , "@Word64", property $ prop_byteSwap_id @Word64
--     ]
