{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: TestUtils
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module TestUtils
( prop_show_read
, prop_json_encode_decode
) where

import qualified Data.Aeson as A

import Test.QuickCheck

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

