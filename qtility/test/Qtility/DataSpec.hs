{-# LANGUAGE TypeApplications #-}

module Qtility.DataSpec where

import Qtility.Data
import RIO
import qualified RIO.Char as Char
import RIO.List.Partial (head, tail)
import qualified RIO.Text as Text
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
      quickCheck $ \(x :: Int) (xs :: [Int]) -> do
        let just = tshow $ Just x
            nothing = tshow $ Nothing @Int
            right = tshow $ Right @String x
            left = tshow $ Left @String @Int "we have nothing"
            xsText = tshow xs
        tReadMaybe just `shouldBe` Just (Just x)
        tReadMaybe nothing `shouldBe` Just (Nothing @Int)
        tReadMaybe right `shouldBe` Just (Right @String x)
        tReadMaybe left `shouldBe` Just (Left @String @Int "we have nothing")
        tReadMaybe xsText `shouldBe` Just xs
  describe "`findM`" $ do
    prop "Should find the first element that satisfies the predicate" $ do
      quickCheck $ \xs ys -> do
        findM ((== 3) >>> pure) (xs <> [3] <> ys) `shouldReturn` Just @Int 3

      quickCheck $ \(xs :: [Int]) -> do
        findM ((== 3) >>> pure) (filter (/= 3) xs) `shouldReturn` Nothing

  describe "`firstRight`" $ do
    prop "Should return the first Right" $ do
      quickCheck $ \(xs :: [Either String Int]) -> do
        let rs = rights xs
            expectedResult = if null rs then Left "NoValue" else Right (head rs)
        firstRight "NoValue" xs `shouldBe` expectedResult

  describe "`{upper,lower}CaseFirst`" $ do
    it "Should uppercase the first letter in a given example" $ do
      upperCaseFirst @Text "hello" `shouldBe` "Hello"
      upperCaseFirst @Text "Hello" `shouldBe` "Hello"
      upperCaseFirst @Text "HELLO" `shouldBe` "HELLO"
      upperCaseFirst @Text "hELLO" `shouldBe` "HELLO"
      upperCaseFirst @Text "h" `shouldBe` "H"
      upperCaseFirst @Text "" `shouldBe` ""

    it "Should lowercase the first letter in a given example" $ do
      lowerCaseFirst @Text "hello" `shouldBe` "hello"
      lowerCaseFirst @Text "Hello" `shouldBe` "hello"
      lowerCaseFirst @Text "HELLO" `shouldBe` "hELLO"
      lowerCaseFirst @Text "hELLO" `shouldBe` "hELLO"
      lowerCaseFirst @Text "h" `shouldBe` "h"
      lowerCaseFirst @Text "" `shouldBe` ""

    prop "Should uppercase the first letter of a `String`" $ do
      quickCheck $ \(x :: String) -> do
        let firstLetter = head x
        upperCaseFirst x `shouldBe` (Char.toUpper firstLetter : tail x)

    prop "Should lowercase the first letter of a `String`" $ do
      quickCheck $ \(x :: String) -> do
        let firstLetter = head x
        lowerCaseFirst x `shouldBe` (Char.toLower firstLetter : tail x)

    prop "Should uppercase the first letter of a `Text`" $ do
      quickCheck $ \x -> do
        let firstLetter = head x
        (x & Text.pack & upperCaseFirst)
          `shouldBe` (Text.singleton (Char.toUpper firstLetter) <> (x & tail & Text.pack))

    prop "Should lowercase the first letter of a `Text`" $ do
      quickCheck $ \x -> do
        let firstLetter = head x
        (x & Text.pack & lowerCaseFirst)
          `shouldBe` (Text.singleton (Char.toLower firstLetter) <> (x & tail & Text.pack))
