-- | Has utility functions for dealing with Amazon's DynamoDB.
module Network.AWS.QAWS.DynamoDB
  ( putItem,
    putItem',
  )
where

import qualified Network.AWS as AWS
import qualified Network.AWS.DynamoDB as DynamoDB
import Network.AWS.QAWS (runAWS')
import Network.AWS.QAWS.DynamoDB.Class (ToAttributeValueMap (..))
import Network.AWS.QAWS.DynamoDB.Types
import Qtility

-- | Puts any value with a valid 'ToAttributeValueMap' instance in a given DynamoDB table. Returns
-- a 'PutItemStatusCode' but can also throw a 'AWS.Error' if there is an AWS operational error.
-- Reads your 'AWS.Env' from your environment.
putItem ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env, ToAttributeValueMap a) =>
  DynamoTableName ->
  a ->
  m PutItemStatusCode
putItem tableName a = do
  awsEnv <- view AWS.environment
  putItem' awsEnv tableName a

-- | Puts any value with a valid 'ToAttributeValueMap' instance in a given DynamoDB table. Returns
-- a 'PutItemStatusCode' but can also throw a 'AWS.Error' if there is an AWS operational error.
putItem' ::
  (MonadUnliftIO m, ToAttributeValueMap a) =>
  AWS.Env ->
  DynamoTableName ->
  a ->
  m PutItemStatusCode
putItem' awsEnv tableName a = do
  let command = DynamoDB.putItem (tableName ^. unwrap) & DynamoDB.piItem .~ toAttributeValueMap a
  ((^. DynamoDB.pirsResponseStatus) >>> PutItemStatusCode) <$> runAWS' awsEnv command
