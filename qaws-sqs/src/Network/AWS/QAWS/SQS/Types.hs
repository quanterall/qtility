{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SQS.Types where

import Control.Lens.TH (makeClassyPrisms, makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Network.AWS as AWS
import Network.AWS.QAWS.Types (ARN)
import Qtility.Environment (FromEnvironmentValue (..))
import RIO

newtype QueueUrl = QueueUrl {_unQueueUrl :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromEnvironmentValue QueueUrl where
  fromEnvironmentValue v = QueueUrl <$> fromEnvironmentValue v

newtype WaitTime = WaitTime {_unWaitTime :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MessageLimit = MessageLimit {_unMessageLimit :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype ReceiptHandle = ReceiptHandle {_unReceiptHandle :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MessageId = MessageId {_unMessageId :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MessageCount = MessageCount {_unMessageCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype DelayedMessageCount = DelayedMessageCount {_unDelayedMessageCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype NotVisibleCount = NotVisibleCount {_unNotVisibleCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A message that is sent to a queue. In contrast to the standard message type in the amazonka
-- libraries, this asserts that message id, receipt handle and body are all present.
data SQSMessage a = SQSMessage
  { _sqsMessageBody :: a,
    _sqsMessageMessageId :: MessageId,
    _sqsMessageReceiptHandle :: ReceiptHandle
  }
  deriving (Eq, Show)

data ReceiveMessageError
  = ReceiveMessageAWSError AWS.Error
  | ReceiveMessageDecodingError String
  | ReceiveMessageNoBody
  | ReceiveMessageNoReceiptHandle
  | ReceiveMessageNoMessageId
  deriving (Show)

instance Exception ReceiveMessageError

data QueueAttributes = QueueAttributes
  { queueAttributesArn :: !(Maybe ARN),
    queueAttributesUrl :: !QueueUrl,
    queueAttributesMessages :: !(Maybe MessageCount),
    queueAttributesDelayedMessages :: !(Maybe DelayedMessageCount),
    queueAttributesNotVisibleMessages :: !(Maybe NotVisibleCount)
  }
  deriving (Eq, Show)

foldMapM
  makeLenses
  [ ''QueueUrl,
    ''WaitTime,
    ''MessageLimit,
    ''ReceiptHandle,
    ''MessageId,
    ''MessageCount,
    ''DelayedMessageCount,
    ''NotVisibleCount,
    ''SQSMessage
  ]

foldMapM makeClassyPrisms [''ReceiveMessageError]
