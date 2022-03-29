module Network.AWS.QAWS.CloudWatchLogs
  ( awsLogInfo,
    awsLogWarn,
    awsLogError,
    awsLog,
    startLoggingQueue,
  )
where

import qualified Data.Aeson.Text as AesonText
import qualified Network.AWS as AWS
import qualified Network.AWS.CloudWatchLogs as CloudWatchLogs
import Network.AWS.QAWS
import Network.AWS.QAWS.CloudWatchLogs.Class
import Network.AWS.QAWS.CloudWatchLogs.Types
import Qtility
import Qtility.Time (getCurrentTimeInMilliseconds)
import Qtility.Time.Types
import qualified RIO.Text.Lazy as LazyText

awsLogInfo ::
  (LogToAws m, ToJSON a) =>
  LogGroupName ->
  LogStreamName ->
  Text ->
  Maybe a ->
  m ()
awsLogInfo = logToAws AWSLogInfo

awsLogWarn ::
  (LogToAws m, ToJSON a) =>
  LogGroupName ->
  LogStreamName ->
  Text ->
  Maybe a ->
  m ()
awsLogWarn = logToAws AWSLogWarn

awsLogError ::
  (LogToAws m, ToJSON a) =>
  LogGroupName ->
  LogStreamName ->
  Text ->
  Maybe a ->
  m ()
awsLogError = logToAws AWSLogError

awsLog ::
  (MonadReader env m, MonadUnliftIO m, HasAWSLoggingQueue env) =>
  AWSLogLevel ->
  AWSLogMessage ->
  m ()
awsLog logLevel logMessage = do
  queue <- view (awsLoggingQueue . unwrap)
  liftIO $
    atomically $
      writeTBMQueue queue $
        AWSLoggingPayload
          { _awsLoggingPayloadLogLevel = logLevel,
            _awsLoggingPayloadLogMessage = logMessage
          }

startLoggingQueue ::
  (MonadReader env m, MonadUnliftIO m, AWS.HasEnv env) =>
  LogGroupName ->
  LogStreamName ->
  AWSQueueSize ->
  m (Async (), AWSLoggingQueue)
startLoggingQueue groupName streamName queueSize = do
  token <- getSequenceToken groupName streamName
  tokenReference <- newIORef token
  awsEnv <- view AWS.environment
  queue <- liftIO $ newTBMQueueIO (queueSize ^. unwrap)
  let loggingState =
        LoggingThreadState
          { _loggingThreadStateGroupName = groupName,
            _loggingThreadStateStreamName = streamName,
            _loggingThreadStateSequenceToken = tokenReference,
            _loggingThreadStateAWSEnv = awsEnv
          }
  asyncThread <- async $ runRIO loggingState $ logLoop groupName streamName queue
  pure (asyncThread, AWSLoggingQueue queue)

logLoop ::
  (MonadReader env m, MonadUnliftIO m, HasSequenceToken env, AWS.HasEnv env) =>
  LogGroupName ->
  LogStreamName ->
  TBMQueue AWSLoggingPayload ->
  m ()
logLoop groupName streamName queue = do
  maybeMessage <- atomically $ readTBMQueue queue
  case maybeMessage of
    Nothing -> pure ()
    Just m -> do
      sendLoggingMessage groupName streamName m
      logLoop groupName streamName queue

sendLoggingMessage ::
  (MonadReader env m, MonadUnliftIO m, HasSequenceToken env, AWS.HasEnv env) =>
  LogGroupName ->
  LogStreamName ->
  AWSLoggingPayload ->
  m ()
sendLoggingMessage groupName streamName payload = do
  tokenReference <- view sequenceToken
  token <- liftIO $ readIORef tokenReference
  now <- liftIO getCurrentTimeInMilliseconds
  let command =
        CloudWatchLogs.putLogEvents
          (groupName ^. unwrap)
          (streamName ^. unwrap)
          logEvents
          & CloudWatchLogs.pleSequenceToken ?~ token ^. unwrap
          & CloudWatchLogs.pleLogGroupName .~ groupName ^. unwrap
          & CloudWatchLogs.pleLogStreamName .~ streamName ^. unwrap
      logLevel = payload ^. awsLoggingPayloadLogLevel
      logEvents = payload ^. awsLoggingPayloadLogMessage & messageToLogEvent now logLevel & pure
  response <- runAWS command
  let nextToken = maybe token SequenceToken $ response ^. CloudWatchLogs.plersNextSequenceToken
  liftIO $ writeIORef tokenReference nextToken

messageToLogEvent :: Timestamp -> AWSLogLevel -> AWSLogMessage -> CloudWatchLogs.InputLogEvent
messageToLogEvent now logLevel logMessage = do
  let value = case logMessage of
        JSONLogMessage v -> v
        PlainLogMessage text -> String text
      eventText = valueToText $ object ["logLevel" .= logLevelToText logLevel, "value" .= value]
  CloudWatchLogs.inputLogEvent (now ^. unwrap . unwrap & fromInteger) eventText

getSequenceToken ::
  (MonadReader env m, MonadUnliftIO m, AWS.HasEnv env) =>
  LogGroupName ->
  LogStreamName ->
  m SequenceToken
getSequenceToken (LogGroupName groupName) (LogStreamName streamName) = do
  let command =
        CloudWatchLogs.describeLogStreams groupName
          & CloudWatchLogs.dlssLogStreamNamePrefix ?~ streamName
  streams <- (^. CloudWatchLogs.dlsrsLogStreams) <$> runAWS command
  case streams of
    stream : _ ->
      case stream ^. CloudWatchLogs.lsUploadSequenceToken of
        Just sequenceTokenValue ->
          pure $ SequenceToken sequenceTokenValue
        Nothing -> error "Invalid sequence token in `getSequenceToken`"
    [] -> error "No streams in `getSequenceToken`"

valueToText :: Value -> Text
valueToText = AesonText.encodeToLazyText >>> LazyText.toStrict

logLevelToText :: AWSLogLevel -> Text
logLevelToText AWSLogInfo = "info"
logLevelToText AWSLogWarn = "warn"
logLevelToText AWSLogError = "error"
