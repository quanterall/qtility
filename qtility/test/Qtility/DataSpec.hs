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
  describe "tReadMaybe" $ do
    it "Parses float values" $ do
      tReadMaybe @Int "1" `shouldBe` Just 1
      tReadMaybe @Float "1.0" `shouldBe` Just 1.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e-2" `shouldBe` Just 0.01
      tReadMaybe @Float "1.0e+2" `shouldBe` Just 100.0
      tReadMaybe @Float "1.0e2" `shouldBe` Just 100.0
    it "Should be able to parse random integer values in containers" $ do
      hedgehog $ do
        x <- forAll Gen.enumBounded
        let just = "Just " <> tshow @Int x
            nothing = "Nothing"
            right = "Right " <> tshow @Int x
            left = "Left " <> tshow @String "we have nothing"
        tReadMaybe @(Maybe Int) just === Just (Just x)
        tReadMaybe @(Maybe Int) nothing === Just Nothing
        tReadMaybe @(Either String Int) right === Just (Right x)
        tReadMaybe @(Either String Int) left === Just (Left "we have nothing")
