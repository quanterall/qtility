module Qtility.Time where

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
