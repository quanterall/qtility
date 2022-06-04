-- | Contains file manipulation utilities.
module Qtility.File where

import Data.Aeson (Value, eitherDecodeStrict')
import Qtility.Exceptions (fromEither)
import Qtility.File.Types
import RIO hiding (fromEither)
import qualified RIO.ByteString as ByteString

withJsonFile :: (MonadIO m, MonadThrow m) => FilePath -> (Value -> m a) -> m a
withJsonFile path f = do
  fileContents <- liftIO $ ByteString.readFile path
  v <- fromEither $ mapLeft (JsonFileDecodingError path) (eitherDecodeStrict' fileContents)
  f v
