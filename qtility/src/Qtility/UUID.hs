module Qtility.UUID
  ( UUID,
    randomUuidV4,
  )
where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import RIO

randomUuidV4 :: (MonadIO m) => m UUID
randomUuidV4 = liftIO nextRandom
