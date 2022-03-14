module Qtility.Time where

import qualified Data.Time.Clock.POSIX as PosixTime
import Qtility.Time.Class
import Qtility.Time.Types
import RIO
import RIO.Time (diffUTCTime, getCurrentTime)

timedM :: (MonadIO m) => m a -> m (TimedResult a)
timedM m = do
  start <- liftIO getCurrentTime
  a <- m
  end <- liftIO getCurrentTime
  let difference = diffUTCTime end start
  pure $ TimedResult difference a

getCurrentTimeInMilliseconds :: (CurrentTime m) => m Timestamp
getCurrentTimeInMilliseconds = do
  currentTime <- getCurrentTimeM
  currentTime
    & PosixTime.utcTimeToPOSIXSeconds
    & realToFrac
    & (*) @Float 1000
    & floor
    & Milliseconds
    & Timestamp
    & pure
