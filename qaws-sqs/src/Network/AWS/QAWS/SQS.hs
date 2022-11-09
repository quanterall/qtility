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
    sendJSONMessages,
    sendJSONMessages',
    sendMessage,
    sendMessage',
    sendMessages,
    sendMessages',
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
import Qtility.UUID (randomUuidV4)
import RIO
import qualified RIO.HashMap as HashMap

-- | Receives messages from a queue with the given 'QueueUrl', waiting for up to 'WaitTime' seconds,
-- for a response and returns a maximum number of 'MessageLimit' messages. This looks for the needed
-- AWS environment in your current environment via 'MonadReader', which makes it ideal for usage in
-- a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'. Throws 'AWS.Error'.
receiveMessages ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m [AWSSQS.Message]
receiveMessages queueUrl waitTime messageLimit = do
  awsEnv <- view AWS.environment
  receiveMessages' awsEnv queueUrl waitTime messageLimit

-- | A version of 'receiveMessages' that automatically constructs a list of 'SQSMessage a', where
-- @a@ is decoded as the body of the message. This looks for the needed AWS environment in your
-- current environment via 'MonadReader', which makes it ideal for usage in a 'MonadReader' based
-- stack (like 'RIO') that implements 'AWS.HasEnv'. Throws 'AWS.Error' if there was an error while
-- interacting with AWS or 'ReceivePayloadError' if we could not decode the message correctly.
receiveWithPayload ::
  ( MonadUnliftIO m,
    MonadReader env m,
    AWS.HasEnv env,
    FromJSON a
  ) =>
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m [SQSMessage a]
receiveWithPayload queueUrl waitTime messageLimit = do
  awsEnv <- view AWS.environment
  receiveWithPayload' awsEnv queueUrl waitTime messageLimit

-- | Deletes the message with 'ReceiptHandle' in the queue with url 'QueueUrl'. This looks for the
-- needed AWS environment in your current environment via 'MonadReader', which makes it ideal for
-- usage in a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'. Throws
-- 'AWS.Error'.
deleteMessage ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) => QueueUrl -> ReceiptHandle -> m ()
deleteMessage queueUrl receiptHandle = do
  awsEnv <- view AWS.environment
  deleteMessage' awsEnv queueUrl receiptHandle

-- | A'la carte version of 'receiveMessages' that takes an environment instead of looking for one
-- in your environment. Throws 'AWS.Error'.
receiveMessages' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m [AWSSQS.Message]
receiveMessages' awsEnv (QueueUrl queueUrl) (WaitTime waitTime) (MessageLimit messageLimit) = do
  let command =
        AWSSQS.receiveMessage queueUrl
          & AWSSQS.rmWaitTimeSeconds ?~ waitTime
          & AWSSQS.rmMaxNumberOfMessages ?~ messageLimit
  (^. AWSSQS.rmrsMessages) <$> runAWS' awsEnv command

-- | A'la carte version of 'receiveWithPayload' that takes an environment instead of looking for one
-- in your environment. Throws 'ReceivePayloadError'.
receiveWithPayload' ::
  (MonadUnliftIO m, FromJSON a) =>
  AWS.Env ->
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m [SQSMessage a]
receiveWithPayload' awsEnv queueUrl waitTime messageLimit = do
  commandResult <-
    mapExceptionM ReceivePayloadAWSError $ receiveMessages' awsEnv queueUrl waitTime messageLimit
  fromEither $ mapM decodeMessage commandResult
  where
    decodeMessage :: (FromJSON a) => AWSSQS.Message -> Either ReceivePayloadError (SQSMessage a)
    decodeMessage m = do
      body <- note ReceivePayloadNoBody $ m ^. AWSSQS.mBody
      _sqsmReceiptHandle <-
        ReceiptHandle <$> note ReceivePayloadNoReceiptHandle (m ^. AWSSQS.mReceiptHandle)
      _sqsmMessageId <- MessageId <$> note ReceivePayloadNoMessageId (m ^. AWSSQS.mMessageId)
      let bytes = encodeUtf8 body
      _sqsmBody <- mapLeft ReceivePayloadDecodingError $ eitherDecodeStrict bytes
      pure $ SQSMessage {_sqsmBody, _sqsmReceiptHandle, _sqsmMessageId}

-- | A'la carte version of 'deleteMessage' that takes an environment instead of looking for one in
-- your environment. Throws 'AWS.Error'.
deleteMessage' ::
  (MonadUnliftIO m) => AWS.Env -> QueueUrl -> ReceiptHandle -> m ()
deleteMessage' awsEnv (QueueUrl queueUrl) (ReceiptHandle receiptHandle) = do
  let command = AWSSQS.deleteMessage queueUrl receiptHandle
  void $ runAWS' awsEnv command

-- | Sends any @a@ with a 'ToJSON' instance to the queue with the given 'QueueUrl'. This looks for
-- the needed AWS environment in your current environment via 'MonadReader', which makes it ideal
-- for usage in a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'. Throws
-- 'AWS.Error'.
sendJSONMessage ::
  ( MonadUnliftIO m,
    MonadReader env m,
    AWS.HasEnv env,
    ToJSON a
  ) =>
  QueueUrl ->
  a ->
  m (Maybe Text)
sendJSONMessage queueUrl a = do
  awsEnv <- view AWS.environment
  sendJSONMessage' awsEnv queueUrl a

-- | A'la carte version of 'sendJSONMessage' that takes an environment instead of looking for one in
-- your environment. Throws 'AWS.Error'.
sendJSONMessage' :: (MonadUnliftIO m, ToJSON a) => AWS.Env -> QueueUrl -> a -> m (Maybe Text)
sendJSONMessage' awsEnv queueUrl a = do
  a & encode & toStrictBytes & decodeUtf8Lenient & sendMessage' awsEnv queueUrl

-- | Sends any @a@s with 'ToJSON' instances to the queue with the given 'QueueUrl'. Uses batching
-- under the hood. This looks for the needed AWS environment in your current environment via
-- 'MonadReader', which makes it ideal for usage in a 'MonadReader' based stack (like 'RIO') that
-- implements 'AWS.HasEnv'. Throws 'AWS.Error'.
sendJSONMessages ::
  ( MonadUnliftIO m,
    MonadReader env m,
    AWS.HasEnv env,
    ToJSON a
  ) =>
  QueueUrl ->
  [a] ->
  m [Text]
sendJSONMessages queueUrl as = do
  awsEnv <- view AWS.environment
  sendJSONMessages' awsEnv queueUrl as

-- | A'la carte version of 'sendJSONMessages' that takes an environment instead of looking for one
-- in your environment. Throws 'AWS.Error'.
sendJSONMessages' :: (MonadUnliftIO m, ToJSON a) => AWS.Env -> QueueUrl -> [a] -> m [Text]
sendJSONMessages' awsEnv queueUrl as = do
  as & fmap (encode >>> toStrictBytes >>> decodeUtf8Lenient) & sendMessages' awsEnv queueUrl

-- | Sends 'Text' to the queue with the given 'QueueUrl'. This looks for the needed AWS environment
-- in your current environment via 'MonadReader', which makes it ideal for usage in a 'MonadReader'
-- based stack (like 'RIO') that implements 'AWS.HasEnv'. Throws 'AWS.Error'.
sendMessage ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  Text ->
  m (Maybe Text)
sendMessage queueUrl message = do
  awsEnv <- view AWS.environment
  sendMessage' awsEnv queueUrl message

-- | A'la carte version of 'sendMessage' that takes an environment instead of looking for one in
-- your environment. Throws 'AWS.Error'.
sendMessage' :: (MonadUnliftIO m) => AWS.Env -> QueueUrl -> Text -> m (Maybe Text)
sendMessage' awsEnv (QueueUrl queueUrl) message = do
  (^. AWSSQS.smrsMessageId) <$> runAWS' awsEnv (AWSSQS.sendMessage queueUrl message)

-- | Sends a list of 'Text' to the queue with the given 'QueueUrl'. This looks for the needed AWS
-- environment in your current environment via 'MonadReader', which makes it ideal for usage in a
-- 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'. Throws 'AWS.Error'.
sendMessages ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  [Text] ->
  m [Text]
sendMessages queueUrl messages = do
  awsEnv <- view AWS.environment
  sendMessages' awsEnv queueUrl messages

-- | A'la carte version of 'sendMessages' that takes an environment instead of looking for one in
-- your environment. Throws 'AWS.Error'.
sendMessages' :: (MonadUnliftIO m) => AWS.Env -> QueueUrl -> [Text] -> m [Text]
sendMessages' awsEnv (QueueUrl queueUrl) messages = do
  withIds <- forM messages $ \m -> do
    uuid <- liftIO randomUuidV4
    pure (tshow uuid, m)
  let entries = map (uncurry AWSSQS.sendMessageBatchRequestEntry) withIds
  (^.. AWSSQS.smbrsSuccessful . traverse . AWSSQS.smbreMessageId)
    <$> runAWS' awsEnv (queueUrl & AWSSQS.sendMessageBatch & AWSSQS.smbEntries .~ entries)

-- | Gets the queue attributes of the queue associated with 'QueueUrl'. This looks for the needed
-- AWS environment in your current environment via 'MonadReader', which makes it ideal for usage in
-- a 'MonadReader' based stack (like 'RIO') that implements 'AWS.HasEnv'. Throws 'AWS.Error'.
getQueueAttributes ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) => QueueUrl -> m QueueAttributes
getQueueAttributes queueUrl = do
  awsEnv <- view AWS.environment
  getQueueAttributes' awsEnv queueUrl

-- | A'la carte version of 'getQueueAttributes' that takes an environment instead of looking for one
-- in your environment. Throws 'AWS.Error'.
getQueueAttributes' :: (MonadUnliftIO m) => AWS.Env -> QueueUrl -> m QueueAttributes
getQueueAttributes' awsEnv queueUrl = do
  let command =
        queueUrl & _unQueueUrl & AWSSQS.getQueueAttributes & AWSSQS.gqaAttributeNames
          .~ [ AWSSQS.QANQueueARN,
               AWSSQS.QANApproximateNumberOfMessages,
               AWSSQS.QANApproximateNumberOfMessagesNotVisible,
               AWSSQS.QANApproximateNumberOfMessagesDelayed
             ]
  createQueueAttributes queueUrl <$> runAWS' awsEnv command

-- | Purges all messages from a queue. This looks for the needed AWS environment in your current
-- environment via 'MonadReader', which makes it ideal for usage in a 'MonadReader' based stack
-- (like 'RIO') that implements 'AWS.HasEnv'. Throws 'AWS.Error'.
purgeQueue :: (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) => QueueUrl -> m ()
purgeQueue queueUrl = do
  awsEnv <- view AWS.environment
  purgeQueue' awsEnv queueUrl

-- | A'la carte version of 'purgeQueue' that takes an environment instead of looking for one in your
-- environment. Throws 'AWS.Error'.
purgeQueue' :: (MonadUnliftIO m) => AWS.Env -> QueueUrl -> m ()
purgeQueue' awsEnv (QueueUrl queueUrl) = do
  void $ runAWS' awsEnv (AWSSQS.purgeQueue queueUrl)

createQueueAttributes :: QueueUrl -> AWSSQS.GetQueueAttributesResponse -> QueueAttributes
createQueueAttributes queueUrl response =
  let m = response ^. AWSSQS.gqarsAttributes
      _qaArn = Arn <$> HashMap.lookup AWSSQS.QANQueueARN m
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
