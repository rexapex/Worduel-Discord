{-# LANGUAGE OverloadedStrings #-}

module GameSpec where

import Test.HSpec
import Test.QuickCheck

spec :: Spec
spec = hspec $ do
    describe "Game" $ do
        it "works" $ do
            True

