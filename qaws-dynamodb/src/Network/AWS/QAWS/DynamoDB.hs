-- | Has utility functions for dealing with Amazon's DynamoDB.
module Network.AWS.QAWS.DynamoDB
  ( putItem,
    putItem',
    getItemParameters,
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

-- | Default structure for 'getItem' parameters. Modify using lenses to set options to not be an
-- empty projection expression and consistent reads.
defaultGetItemParameters ::
  DynamoTableName -> HashMap Text DynamoDB.AttributeValue -> GetItemParameters
defaultGetItemParameters tableName key =
  GetItemParameters
    { _getItemParametersTableName = tableName,
      _getItemParametersKey = key,
      _getItemParametersProjectionExpression = [],
      _getItemParametersConsistentRead = True
    }

-- | Gets an item from Dynamo according to the projection expression (or just all keys if left
-- empty) in the 'GetItemParameters' specified. The type that is fetched has to have an instance of
-- 'FromAttributeValueMap' to be able to convert the result to the desired type, which means it has
-- to be a type that can be converted to from a 'HashMap' of 'Text' and 'AttributeValue'. The
-- utility class 'FromAttributeValue' can be used to define such an instance easily.
getItem' ::
  (MonadUnliftIO m, MonadThrow m, FromAttributeValueMap a) =>
  AWS.Env ->
  GetItemParameters ->
  m a
getItem' awsEnv parameters = do
  let command =
        DynamoDB.getItem (parameters ^. getItemParametersTableName . unwrap)
          & DynamoDB.giKey .~ parameters ^. getItemParametersKey
          & DynamoDB.giProjectionExpression .~ projectionExpression
          & DynamoDB.giConsistentRead ?~ parameters ^. getItemParametersConsistentRead
      projectionExpression =
        if not (null (parameters ^. getItemParametersProjectionExpression))
          then parameters ^. getItemParametersProjectionExpression & Text.intercalate "," & Just
          else Nothing
  fromEitherM $
    ((^. DynamoDB.girsItem) >>> fromAttributeValueMap >>> mapLeft GetItemDecodingError)
      <$> runAWS' awsEnv command

-- | Gets an item from Dynamo according to the projection expression (or just all keys if left
-- empty) in the 'GetItemParameters' specified. The type that is fetched has to have an instance of
-- 'FromAttributeValueMap' to be able to convert the result to the desired type, which means it has
-- to be a type that can be converted to from a 'HashMap' of 'Text' and 'AttributeValue'. The
-- utility class 'FromAttributeValue' can be used to define such an instance easily. This reads
-- your AWS environment from your 'MonadReader' environment.
getItem ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env, FromAttributeValueMap a) =>
  GetItemParameters ->
  m a
getItem parameters = do
  awsEnv <- view AWS.environment
  getItem' awsEnv parameters
