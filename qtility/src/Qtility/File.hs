-- | Contains file manipulation utilities.
module Qtility.File where

import Data.Aeson (Value, eitherDecodeStrict')
import Qtility.Exceptions (fromEither)
import Qtility.File.Types
import RIO hiding (fromEither)
import qualified RIO.ByteString as ByteString

-- | Reads a file and attempts to decode the contents as a JSON value, throwing a
-- 'JsonFileDecodingError' if it cannot do so, then runs the given closure on the value.
withJsonFile :: (MonadIO m, MonadThrow m) => FilePath -> (Value -> m a) -> m a
withJsonFile path f = do
  fileContents <- liftIO $ ByteString.readFile path
  v <- fromEither $ mapLeft (JsonFileDecodingError path) (eitherDecodeStrict' fileContents)
  f v
