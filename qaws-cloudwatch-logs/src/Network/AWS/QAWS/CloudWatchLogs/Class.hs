module Network.AWS.QAWS.CloudWatchLogs.Class where

import Network.AWS.QAWS.CloudWatchLogs.Types
import Qtility

class (Monad m) => LogToAws m where
  logToAws :: (ToJSON a) => AWSLogLevel -> LogGroupName -> LogStreamName -> Text -> Maybe a -> m ()
