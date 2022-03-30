-- | Has utility functions for dealing with Amazon's SNS.
module Network.AWS.QAWS.SNS
  ( publish,
    publish',
  )
where

import qualified Network.AWS as AWS
import Network.AWS.QAWS (runAWS')
import Network.AWS.QAWS.SNS.Types
import qualified Network.AWS.SNS as SNS
import Qtility

-- | Publishes any structure that has a 'ToJSON' instance to the topic with the given 'TopicARN'.
-- If a general AWS error happens throws 'AWS.Error'. The returned 'PublishStatusCode' can also be
-- checked for any non-exceptional errors. This reads your 'AWS.Env' from your reader environment.
publish ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env, ToJSON a) =>
  TopicARN ->
  a ->
  m PublishStatusCode
publish topicArn a = do
  awsEnv <- view AWS.environment
  publish' awsEnv topicArn a

-- | Publishes any structure that has a 'ToJSON' instance to the topic with the given 'TopicARN'.
-- If a general AWS error happens throws 'AWS.Error'. The returned 'PublishStatusCode' can also be
-- checked for any non-exceptional errors. This takes the 'AWS.Env' as a parameter and can thus be
-- used in a more a'la carte way.
publish' :: (MonadUnliftIO m, ToJSON a) => AWS.Env -> TopicARN -> a -> m PublishStatusCode
publish' awsEnv topicArn a = do
  let message = a & encode & toStrictBytes & decodeUtf8Lenient
      command = SNS.publish message & SNS.pTopicARN ?~ (topicArn ^. unwrap)
  ((^. SNS.prsResponseStatus) >>> PublishStatusCode) <$> runAWS' awsEnv command
