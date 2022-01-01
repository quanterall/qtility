{-# LANGUAGE TemplateHaskell #-}

module JSONSpec where

import Data.Aeson (Value, eitherDecode, encode)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Qtility.JSON
import RIO
import qualified RIO.Map as Map
import Test.Hspec
import Test.Hspec.Hedgehog

data TestType = TestType {_testTypeField1 :: Int, _testTypeField2 :: String}
  deriving (Eq, Show, Generic)

deriveJSON ''TestType

spec :: Spec
spec = do
  describe "TestType" $ do
    it "should be able to be serialized and deserialized" $ do
      hedgehog $ do
        _testTypeField1 <- forAll Gen.enumBounded
        _testTypeField2 <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
        let testType = TestType {_testTypeField1, _testTypeField2}
            json = encode testType
            testType' = eitherDecode json
        testType' === Right testType

    it "should contain the correct field names" $ do
      let testType = TestType {_testTypeField1 = 1, _testTypeField2 = "test"}
          json = encode testType
          decoded = eitherDecode json :: Either String (Map Text Value)
      case decoded of
        Left _ -> expectationFailure "Failed to decode JSON"
        Right decodedMap -> do
          decodedMap `shouldHaveKey` "field1"
          decodedMap `shouldHaveKey` "field2"

shouldHaveKey :: Map Text a -> Text -> Expectation
shouldHaveKey m key = do
  Map.member key m `shouldBe` True
