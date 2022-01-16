{-# LANGUAGE TypeApplications #-}

module Qtility.DataSpec where

import qualified Hedgehog.Gen as Gen
import Qtility.Data
import RIO
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "`note`" $ do
    it "Should annotate `Just` & `Nothing` correctly" $ do
      hedgehog $ do
        x <- forAll Gen.enumBounded
        let hasValue = note @String @Int "x" (Just x)
            doesNotHaveValue = note @String @Int "we have nothing" Nothing
        hasValue === Right x
        doesNotHaveValue === Left "we have nothing"
  describe "`hush`" $ do
    it "Should silence `Left`s and keep `Right`s" $ do
      hedgehog $ do
        x <- forAll Gen.enumBounded
        let hasValue = hush @String @Int $ Right x
            doesNotHaveValue = hush @String @Int $ Left "we have nothing"
        hasValue === Just x
        doesNotHaveValue === Nothing
