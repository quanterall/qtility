-- | Has utility functions for dealing with Amazon's DynamoDB.
module Network.AWS.QAWS.DynamoDB
  ( putItem,
    putItem',
    defaultGetItemOptions,
    getItem,
    getItem',
  )
where

import qualified Network.AWS as AWS
import qualified Network.AWS.DynamoDB as DynamoDB
import Network.AWS.QAWS (runAWS')
import Network.AWS.QAWS.DynamoDB.Class (FromAttributeValueMap (..), ToAttributeValueMap (..))
import Network.AWS.QAWS.DynamoDB.Types
import Qtility
import qualified RIO.Text as Text

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

-- | Gets an item from Dynamo according to the projection expression (or just all keys if left
-- empty) in the 'GetItemOptions' specified. The type that is fetched has to have an instance of
-- 'FromAttributeValueMap' to be able to convert the result to the desired type, which means it has
-- to be a type that can be converted to from a 'HashMap' of 'Text' and 'AttributeValue'. The
-- utility class 'FromAttributeValue' can be used to define such an instance easily. This reads
-- your AWS environment from your 'MonadReader' environment.
getItem ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env, FromAttributeValueMap a) =>
  DynamoTableName ->
  DynamoKey ->
  GetItemOptions ->
  m a
getItem tableName key parameters = do
  awsEnv <- view AWS.environment
  getItem' awsEnv tableName key parameters

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

-- | Default structure for 'getItem' options. Modify using lenses to set options to not be an empty
-- projection expression and consistent reads.
defaultGetItemOptions :: GetItemOptions
defaultGetItemOptions =
  GetItemOptions
    { _getItemOptionsProjectionExpression = [],
      _getItemOptionsConsistentRead = True
    }

-- | Gets an item from Dynamo according to the projection expression (or just all keys if left
-- empty) in the 'GetItemOptions' specified. The type that is fetched has to have an instance of
-- 'FromAttributeValueMap' to be able to convert the result to the desired type, which means it has
-- to be a type that can be converted to from a 'HashMap' of 'Text' and 'AttributeValue'. The
-- utility class 'FromAttributeValue' can be used to define such an instance easily.
getItem' ::
  (MonadUnliftIO m, MonadThrow m, FromAttributeValueMap a) =>
  AWS.Env ->
  DynamoTableName ->
  DynamoKey ->
  GetItemOptions ->
  m a
getItem' awsEnv tableName key parameters = do
  let command =
        DynamoDB.getItem (tableName ^. unwrap)
          & DynamoDB.giKey .~ (key ^. unwrap)
          & DynamoDB.giProjectionExpression .~ projectionExpression
          & DynamoDB.giConsistentRead ?~ parameters ^. getItemOptionsConsistentRead
      projectionExpression =
        if not (null (parameters ^. getItemOptionsProjectionExpression))
          then parameters ^. getItemOptionsProjectionExpression & Text.intercalate "," & Just
          else Nothing
  fromEitherM $
    ((^. DynamoDB.girsItem) >>> fromAttributeValueMap >>> mapLeft GetItemDecodingError)
      <$> runAWS' awsEnv command
