-- | Contains file manipulation utilities.
module Qtility.File where

import Data.Aeson (Value, eitherDecodeStrict')
-- import Qtility.Exceptions (fromEitherM)
import Qtility.File.Types
import RIO
import qualified RIO.ByteString as ByteString

withJsonFile :: (MonadIO m, MonadThrow m) => FilePath -> (Value -> m a) -> m a
withJsonFile path f = do
  v <-
    fromEitherM $
      mapLeft
        (JsonFileDecodingError path)
        (eitherDecodeStrict' <$> ByteString.readFile path)
  f v
