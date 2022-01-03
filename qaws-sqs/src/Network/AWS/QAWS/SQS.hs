-- | Has utility functions for dealing with Amazon's Simple Queue Service (SQS).
module Network.AWS.QAWS.SQS
  ( receiveMessages,
    receiveMessages',
    receiveWithPayload,
    receiveWithPayload',
    deleteMessage,
    deleteMessage',
    sendJSONMessage,
    sendJSONMessage',
    sendMessage,
    sendMessage',
    getQueueAttributes,
    getQueueAttributes',
    purgeQueue,
    purgeQueue',
  )
where

import Control.Lens ((?~))
import Control.Lens.Prism
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode)
import qualified Network.AWS as AWS
import Network.AWS.QAWS
import Network.AWS.QAWS.SQS.Types
import Network.AWS.QAWS.Types
import qualified Network.AWS.SQS as AWSSQS
import Qtility.Data (fromText, note)
import RIO
import qualified RIO.HashMap as HashMap

-- | Receives messages from a queue with the given 'QueueUrl', waiting for up to 'WaitTime' seconds,
-- for a response and returns a maximum number of 'MessageLimit' messages. This looks for the needed
-- AWS environment in your current environment via 'MonadReader', which makes it ideal for usage in
-- a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'.
receiveMessages ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either AWS.Error [AWSSQS.Message])
receiveMessages queueUrl waitTime messageLimit = do
  awsEnv <- view AWS.environment
  receiveMessages' awsEnv queueUrl waitTime messageLimit

-- | A version of 'receiveMessages' that automatically constructs a list of 'SQSMessage a', where
-- @a@ is decoded as the body of the message. This looks for the needed AWS environment in your
-- current environment via 'MonadReader', which makes it ideal for usage in a 'MonadReader' based
-- stack (like 'RIO') that implements 'AWS.HasEnv'.
receiveWithPayload ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env, FromJSON a) =>
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either ReceiveMessageError [SQSMessage a])
receiveWithPayload queueUrl waitTime messageLimit = do
  awsEnv <- view AWS.environment
  receiveWithPayload' awsEnv queueUrl waitTime messageLimit

-- | Deletes the message with 'ReceiptHandle' in the queue with url 'QueueUrl'. This looks for the
-- needed AWS environment in your current environment via 'MonadReader', which makes it ideal for
-- usage in a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'.
deleteMessage ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  ReceiptHandle ->
  m (Either AWS.Error ())
deleteMessage queueUrl receiptHandle = do
  awsEnv <- view AWS.environment
  deleteMessage' awsEnv queueUrl receiptHandle

-- | A'la carte version of 'receiveMessages' that takes an environment instead of looking for one
-- in your environment.
receiveMessages' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either AWS.Error [AWSSQS.Message])
receiveMessages' awsEnv (QueueUrl queueUrl) (WaitTime waitTime) (MessageLimit messageLimit) = do
  let command =
        AWSSQS.receiveMessage queueUrl
          & AWSSQS.rmWaitTimeSeconds ?~ waitTime
          & AWSSQS.rmMaxNumberOfMessages ?~ messageLimit
  commandResult <- tryRunAWS' awsEnv command
  either (Left >>> pure) ((^. AWSSQS.rmrsMessages) >>> Right >>> pure) commandResult

-- | A'la carte version of 'receiveWithPayload' that takes an environment instead of looking for one
-- in your environment.
receiveWithPayload' ::
  (MonadUnliftIO m, FromJSON a) =>
  AWS.Env ->
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either ReceiveMessageError [SQSMessage a])
receiveWithPayload' awsEnv queueUrl waitTime messageLimit = do
  commandResult <-
    mapLeft ReceiveMessageAWSError
      <$> receiveMessages' awsEnv queueUrl waitTime messageLimit
  pure $ either Left (mapM decodeMessage) commandResult
  where
    decodeMessage :: (FromJSON a) => AWSSQS.Message -> Either ReceiveMessageError (SQSMessage a)
    decodeMessage m = do
      body <- note ReceiveMessageNoBody $ m ^. AWSSQS.mBody
      _sqsmReceiptHandle <-
        ReceiptHandle <$> note ReceiveMessageNoReceiptHandle (m ^. AWSSQS.mReceiptHandle)
      _sqsmMessageId <- MessageId <$> note ReceiveMessageNoMessageId (m ^. AWSSQS.mMessageId)
      let bytes = encodeUtf8 body
      _sqsmBody <- mapLeft ReceiveMessageDecodingError $ eitherDecodeStrict bytes
      pure $ SQSMessage {_sqsmBody, _sqsmReceiptHandle, _sqsmMessageId}

-- | A'la carte version of 'deleteMessage' that takes an environment instead of looking for one in
-- your environment.
deleteMessage' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  QueueUrl ->
  ReceiptHandle ->
  m (Either AWS.Error ())
deleteMessage' awsEnv (QueueUrl queueUrl) (ReceiptHandle receiptHandle) = do
  let command = AWSSQS.deleteMessage queueUrl receiptHandle
  void <$> tryRunAWS' awsEnv command

-- | Sends any @a@ with a 'ToJSON' instance to the queue with the given 'QueueUrl'. This looks for
-- the needed AWS environment in your current environment via 'MonadReader', which makes it ideal
-- for usage in a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'.
sendJSONMessage ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env, ToJSON a) =>
  QueueUrl ->
  a ->
  m (Either AWS.Error (Maybe Text))
sendJSONMessage queueUrl a = do
  awsEnv <- view AWS.environment
  sendJSONMessage' awsEnv queueUrl a

-- | A'la carte version of 'sendJSONMessage' that takes an environment instead of looking for one in
-- your environment.
sendJSONMessage' ::
  (MonadUnliftIO m, ToJSON a) =>
  AWS.Env ->
  QueueUrl ->
  a ->
  m (Either AWS.Error (Maybe Text))
sendJSONMessage' awsEnv queueUrl a = do
  a & encode & toStrictBytes & decodeUtf8Lenient & sendMessage' awsEnv queueUrl

-- | Sends 'Text' to the queue with the given 'QueueUrl'. This looks for the needed AWS environment
-- in your current environment via 'MonadReader', which makes it ideal for usage in a 'MonadReader'
-- based stack (like 'RIO') that implements 'AWS.HasEnv'.
sendMessage ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  Text ->
  m (Either AWS.Error (Maybe Text))
sendMessage queueUrl message = do
  awsEnv <- view AWS.environment
  sendMessage' awsEnv queueUrl message

-- | A'la carte version of 'sendMessage' that takes an environment instead of looking for one in
-- your environment.
sendMessage' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  QueueUrl ->
  Text ->
  m (Either AWS.Error (Maybe Text))
sendMessage' awsEnv (QueueUrl queueUrl) message = do
  fmap (^. AWSSQS.smrsMessageId) <$> tryRunAWS' awsEnv (AWSSQS.sendMessage queueUrl message)

-- | Gets the queue attributes of the queue associated with 'QueueUrl'. This looks for the needed
-- AWS environment in your current environment via 'MonadReader', which makes it ideal for usage in
-- a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'.
getQueueAttributes ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  m (Either AWS.Error QueueAttributes)
getQueueAttributes queueUrl = do
  awsEnv <- view AWS.environment
  getQueueAttributes' awsEnv queueUrl

-- | A'la carte version of 'getQueueAttributes' that takes an environment instead of looking for one
-- in your environment.
getQueueAttributes' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  QueueUrl ->
  m (Either AWS.Error QueueAttributes)
getQueueAttributes' awsEnv queueUrl = do
  let command =
        queueUrl & _unQueueUrl & AWSSQS.getQueueAttributes & AWSSQS.gqaAttributeNames
          .~ [ AWSSQS.QANQueueARN,
               AWSSQS.QANApproximateNumberOfMessages,
               AWSSQS.QANApproximateNumberOfMessagesNotVisible,
               AWSSQS.QANApproximateNumberOfMessagesDelayed
             ]
  fmap (createQueueAttributes queueUrl) <$> tryRunAWS' awsEnv command

createQueueAttributes :: QueueUrl -> AWSSQS.GetQueueAttributesResponse -> QueueAttributes
createQueueAttributes queueUrl response =
  let m = response ^. AWSSQS.gqarsAttributes
      _qaArn = ARN <$> HashMap.lookup AWSSQS.QANQueueARN m
      _qaMessages =
        MessageCount <$> (HashMap.lookup AWSSQS.QANApproximateNumberOfMessages m ^? _Just . fromText)
      -- MessageCount <$> (HashMap.lookup AWSSQS.QANApproximateNumberOfMessages m >>= tReadMaybe)
      _qaDelayedMessages =
        DelayedMessageCount
          <$> (HashMap.lookup AWSSQS.QANApproximateNumberOfMessagesDelayed m ^? _Just . fromText)
      _qaNotVisibleMessages =
        NotVisibleCount
          <$> (HashMap.lookup AWSSQS.QANApproximateNumberOfMessagesNotVisible m ^? _Just . fromText)
   in QueueAttributes
        { _qaArn,
          _qaUrl = queueUrl,
          _qaMessages,
          _qaDelayedMessages,
          _qaNotVisibleMessages
        }

-- | Purges all messages from a queue. This looks for the needed AWS environment in your current
-- environment via 'MonadReader', which makes it ideal for usage in a 'MonadReader' based stack
-- (like 'RIO') that implements 'AWS.HasEnv'.
purgeQueue ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  m (Either AWS.Error ())
purgeQueue queueUrl = do
  awsEnv <- view AWS.environment
  purgeQueue' awsEnv queueUrl

-- | A'la carte version of 'purgeQueue' that takes an environment instead of looking for one in your
-- environment.
purgeQueue' :: (MonadUnliftIO m) => AWS.Env -> QueueUrl -> m (Either AWS.Error ())
purgeQueue' awsEnv (QueueUrl queueUrl) = do
  void <$> tryRunAWS' awsEnv (AWSSQS.purgeQueue queueUrl)
