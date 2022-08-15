{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SQS.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Network.AWS as AWS
import Network.AWS.QAWS.Types (Arn)
import Qtility.Environment (FromEnvironmentValue (..))
import Qtility.TH.Optics (makeClassyException)
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
  { _sqsmBody :: a,
    _sqsmMessageId :: MessageId,
    _sqsmReceiptHandle :: ReceiptHandle
  }
  deriving (Eq, Show)

data ReceivePayloadError
  = ReceivePayloadAWSError AWS.Error
  | ReceivePayloadDecodingError String
  | ReceivePayloadNoBody
  | ReceivePayloadNoReceiptHandle
  | ReceivePayloadNoMessageId
  deriving (Show)

data QueueAttributes = QueueAttributes
  { _qaArn :: !(Maybe Arn),
    _qaUrl :: !QueueUrl,
    _qaMessages :: !(Maybe MessageCount),
    _qaDelayedMessages :: !(Maybe DelayedMessageCount),
    _qaNotVisibleMessages :: !(Maybe NotVisibleCount)
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
    ''SQSMessage,
    ''QueueAttributes
  ]

foldMapM makeClassyException [''ReceivePayloadError]
