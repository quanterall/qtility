{-# LANGUAGE TypeApplications #-}

module Qtility.DataSpec where

import Qtility.Data
import RIO
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "`note`" $ do
    prop "Should annotate `Just` & `Nothing` correctly" $ do
      quickCheck $ \x -> do
        let hasValue = note @String @Int "x" (Just x)
            doesNotHaveValue = note @String @Int "we have nothing" Nothing
        hasValue `shouldBe` Right x
        doesNotHaveValue `shouldBe` Left "we have nothing"
  describe "`hush`" $ do
    prop "Should silence `Left`s and keep `Right`s" $ do
      quickCheck $ \x -> do
        let hasValue = hush @String @Int $ Right x
            doesNotHaveValue = hush @String @Int $ Left "we have nothing"
        hasValue `shouldBe` Just x
        doesNotHaveValue `shouldBe` Nothing
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
    prop "Should be able to parse random integer values in containers" $ do
      quickCheck $ \x -> do
        let just = "Just " <> tshow @Int x
            nothing = "Nothing"
            right = "Right " <> tshow @Int x
            left = "Left " <> tshow @String "we have nothing"
        tReadMaybe @(Maybe Int) just `shouldBe` Just (Just x)
        tReadMaybe @(Maybe Int) nothing `shouldBe` Just Nothing
        tReadMaybe @(Either String Int) right `shouldBe` Just (Right x)
        tReadMaybe @(Either String Int) left `shouldBe` Just (Left "we have nothing")
  describe "`findM`" $ do
    prop "Should find the first element that satisfies the predicate" $ do
      quickCheck $ \xs ys -> do
        findM ((== 3) >>> pure) (xs <> [3] <> ys) `shouldReturn` Just @Int 3

      quickCheck $ \(xs :: [Int]) -> do
        findM ((== 3) >>> pure) (filter (/= 3) xs) `shouldReturn` Nothing
