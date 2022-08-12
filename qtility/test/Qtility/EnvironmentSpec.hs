{-# LANGUAGE TypeApplications #-}

module Qtility.EnvironmentSpec where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Qtility.Environment
import Qtility.Environment.Types
import RIO
import RIO.Directory (removeFile)
import RIO.FilePath ((</>))
import RIO.List (isInfixOf)
import qualified RIO.Map as Map
import System.Environment (setEnv)
import System.IO (hPutStrLn)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "`readEnvironmentVariable`" $ do
    it "Fails with an error if the environment variable is not set" $ do
      readEnvironmentVariable @String (EnvironmentKey "NOT_SET")
        `shouldThrow` (== ReadEnvironmentMissingValue (EnvironmentKey "NOT_SET"))

    it "Succeeds if we set the variable first" $ do
      let key = EnvironmentKey "SET"
      setEnv (_unEnvironmentKey key) "VALUE"
      readEnvironmentVariable @String key `shouldReturn` "VALUE"

    it "Can read `Text` values correctly" $ do
      let key = EnvironmentKey "TEXT"
      setEnv (_unEnvironmentKey key) "VALUE"
      readEnvironmentVariable @Text key `shouldReturn` "VALUE"

    it "Can read `Int` values correctly" $ do
      let key = EnvironmentKey "INT"
      setEnv (_unEnvironmentKey key) "42"
      readEnvironmentVariable @Int key `shouldReturn` 42

    it "Can read `Double` values correctly" $ do
      let key = EnvironmentKey "DOUBLE"
      setEnv (_unEnvironmentKey key) "42"
      readEnvironmentVariable @Double key `shouldReturn` 42.0

    it "Can read `Bool` values correctly ('true')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "true"
      readEnvironmentVariable key `shouldReturn` True

    it "Can read `Bool` values correctly ('false')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "false"
      readEnvironmentVariable key `shouldReturn` False

    it "Can read `Bool` values correctly ('yes')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "yes"
      readEnvironmentVariable key `shouldReturn` True

    it "Can read `Bool` values correctly ('no')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "no"
      readEnvironmentVariable key `shouldReturn` False

    it "Can read `Bool` values correctly ('on')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "on"
      readEnvironmentVariable key `shouldReturn` True

    it "Can read `Bool` values correctly ('off')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "off"
      readEnvironmentVariable key `shouldReturn` False

    it "Can read any `Int` value from the environment" $ do
      let key = EnvironmentKey "ANY_INT"
      hedgehog $ do
        value <- forAll Gen.enumBounded
        liftIO $ setEnv (_unEnvironmentKey key) (show @Int value)
        result <- liftIO $ readEnvironmentVariable key
        result === value

    it "Can read any `Double` value from the environment" $ do
      let key = EnvironmentKey "ANY_DOUBLE"
      hedgehog $ do
        value <- forAll $ Gen.double $ Range.exponentialFloat 1 512
        liftIO $ setEnv (_unEnvironmentKey key) (show @Double value)
        result <- liftIO $ readEnvironmentVariable key
        result === value

    it "Can read any `Text` value from the environment" $ do
      let key = EnvironmentKey "ANY_TEXT"
      anyTextProp key

anyTextProp :: EnvironmentKey -> PropertyT IO ()
anyTextProp key = hedgehog $ do
  value <-
    -- Filter this so that we don't try to get a value that contains @NUL@
    forAll $ Gen.string (Range.linear 1 512) Gen.unicode & Gen.filter (("\NUL" `isInfixOf`) >>> not)
  liftIO $ setEnv (_unEnvironmentKey key) value
  result <- liftIO $ readEnvironmentVariable key
  result === value

genEnvMap :: Gen (Map EnvironmentKey String)
genEnvMap = do
  keyCount <- Gen.int (Range.linear 0 100)
  keys <- Gen.list (Range.singleton keyCount) genKey
  values <- Gen.list (Range.singleton keyCount) genValue
  pure $ Map.fromList $ zip keys values
  where
    genKey = do
      startingCharacter <- Gen.element ['A' .. 'Z']
      ((startingCharacter :) >>> EnvironmentKey)
        <$> Gen.string (Range.linear 1 50) Gen.alphaNum
        & Gen.filter (_unEnvironmentKey >>> ("\"" `isInfixOf`) >>> not)
    genValue =
      Gen.string (Range.linear 1 50) Gen.unicode
        & Gen.filter (("\"" `isInfixOf`) >>> not)
