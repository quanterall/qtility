{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.CloudWatchLogs.Types where

import qualified Network.AWS as AWS
import Qtility

data AWSLoggingPayload = AWSLoggingPayload
  { _awsLoggingPayloadLogLevel :: !AWSLogLevel,
    _awsLoggingPayloadLogMessage :: !AWSLogMessage
  }
  deriving (Eq, Show, Generic)

data LoggingThreadState = LoggingThreadState
  { _loggingThreadStateGroupName :: !LogGroupName,
    _loggingThreadStateStreamName :: !LogStreamName,
    _loggingThreadStateSequenceToken :: !(IORef SequenceToken),
    _loggingThreadStateAWSEnv :: !AWS.Env
  }
  deriving (Generic)

data AWSLogMessage
  = JSONLogMessage Value
  | PlainLogMessage Text
  deriving (Eq, Show, Generic)

data AWSLogLevel
  = AWSLogInfo
  | AWSLogWarn
  | AWSLogError
  deriving (Eq, Show, Generic)

newtype AWSLoggingQueue = AWSLoggingQueue {unwrapAWSLoggingQueue :: TBMQueue AWSLoggingPayload}
  deriving (Generic)

newtype SequenceToken = SequenceToken {unwrapSequenceToken :: Text}
  deriving (Eq, Show, Generic)

newtype LogGroupName = LogGroupName {unLogGroupName :: Text}
  deriving (Eq, Show, Generic)

newtype LogStreamName = LogStreamName {unLogStreamName :: Text}
  deriving (Eq, Show, Generic)

newtype AWSQueueSize = AWSQueueSize {unAWSQueueSize :: Int}
  deriving (Eq, Show, Generic)

class HasSequenceToken a where
  sequenceToken :: Lens' a (IORef SequenceToken)

class HasAWSLoggingQueue env where
  awsLoggingQueue :: Lens' env AWSLoggingQueue

foldMapM
  makeWrapped
  [ ''AWSLoggingQueue,
    ''SequenceToken,
    ''LogGroupName,
    ''LogStreamName,
    ''AWSQueueSize
  ]

foldMapM makeLenses [''AWSLoggingPayload, ''LoggingThreadState]

instance HasSequenceToken (IORef SequenceToken) where
  sequenceToken = id

instance HasSequenceToken LoggingThreadState where
  sequenceToken = loggingThreadStateSequenceToken

instance HasAWSLoggingQueue AWSLoggingQueue where
  awsLoggingQueue = id

instance AWS.HasEnv LoggingThreadState where
  environment = loggingThreadStateAWSEnv
