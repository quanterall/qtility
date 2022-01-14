{-# LANGUAGE TypeApplications #-}

module Qtility.TimeSpec where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Qtility
import Qtility.Time
import Qtility.Time.Types
import RIO.Time (NominalDiffTime)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "timedM" $ do
    modifyMaxSuccess (const 25) $
      it "Times randomly delayed actions correctly" $ do
        hedgehog $ do
          value <- forAll $ Gen.int (Range.linear 0 100)
          time <- forAll randomNominalDiffTime
          result <- timedM $ do
            threadDelay $ toMicroseconds time
            pure value
          assertCloseEnough 0.001 (result ^. trTime) time
          result ^. trValue === value

randomNominalDiffTime :: Gen NominalDiffTime
randomNominalDiffTime =
  realToFrac @Double <$> Gen.realFloat (Range.linearFrac 0 0.3)

toMicroseconds :: NominalDiffTime -> Int
toMicroseconds = (* 1000000) >>> round

assertCloseEnough :: (MonadTest m, Num a, Ord a, Show a) => a -> a -> a -> m ()
assertCloseEnough limit a = diff a (numberIsCloseEnough limit)

numberIsCloseEnough :: (Num a, Ord a) => a -> a -> a -> Bool
numberIsCloseEnough limit a b = abs (a - b) < limit
