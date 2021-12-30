{-# LANGUAGE TypeApplications #-}

module EnvironmentSpec where

import Qtility.Environment
import Qtility.Environment.Types
import RIO
import System.Environment (setEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "`readEnvironmentVariable`" $ do
    it "Fails with an error if the environment variable is not set" $ do
      readEnvironmentVariable @String (EnvironmentKey "NOT_SET")
        `shouldThrow` (== ReadEnvironmentMissingValue (EnvironmentKey "NOT_SET"))

    it "Succeeds if we set the variable first" $ do
      let key = EnvironmentKey "SET"
      setEnv (_unEnvironmentKey key) "VALUE"
      result <- readEnvironmentVariable @String key
      result `shouldBe` "VALUE"

    it "Can read `Text` values correctly" $ do
      let key = EnvironmentKey "TEXT"
      setEnv (_unEnvironmentKey key) "VALUE"
      result <- readEnvironmentVariable @Text key
      result `shouldBe` "VALUE"

    it "Can read `Int` values correctly" $ do
      let key = EnvironmentKey "INT"
      setEnv (_unEnvironmentKey key) "42"
      result <- readEnvironmentVariable @Int key
      result `shouldBe` 42

    it "Can read `Double` values correctly" $ do
      let key = EnvironmentKey "DOUBLE"
      setEnv (_unEnvironmentKey key) "42.0"
      result <- readEnvironmentVariable @Double key
      result `shouldBe` 42.0

    it "Can read `Bool` values correctly ('true')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "true"
      result <- readEnvironmentVariable key
      result `shouldBe` True

    it "Can read `Bool` values correctly ('false')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "false"
      result <- readEnvironmentVariable key
      result `shouldBe` False

    it "Can read `Bool` values correctly ('yes')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "yes"
      result <- readEnvironmentVariable key
      result `shouldBe` True

    it "Can read `Bool` values correctly ('no')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "no"
      result <- readEnvironmentVariable key
      result `shouldBe` False

    it "Can read `Bool` values correctly ('on')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "on"
      result <- readEnvironmentVariable key
      result `shouldBe` True

    it "Can read `Bool` values correctly ('off')" $ do
      let key = EnvironmentKey "BOOL"
      setEnv (_unEnvironmentKey key) "off"
      result <- readEnvironmentVariable key
      result `shouldBe` False
