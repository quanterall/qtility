module Qtility.Time.Class where

import qualified Data.Time.Clock.POSIX as PosixTime
import RIO
import RIO.Time (UTCTime)

class (Monad m) => CurrentTime m where
  getCurrentTimeM :: m UTCTime

instance CurrentTime IO where
  getCurrentTimeM = PosixTime.getCurrentTime
