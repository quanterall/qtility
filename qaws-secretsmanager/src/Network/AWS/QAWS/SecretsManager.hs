-- | Has utility functions for dealing with Amazon's SecretsManager.
module Network.AWS.QAWS.SecretsManager
  ( getSecretValueAs,
    getSecretValueAs',
    getSecretValue,
    getSecretValue',
    createConnectionPoolForSecretArn,
    createConnectionPoolForSecretArn',
    createConnectionPoolForSecretArnWithDbName,
    createConnectionPoolForSecretArnWithDbName',
  )
where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Network.AWS as AWS
import Network.AWS.QAWS (runAWS')
import Network.AWS.QAWS.SecretsManager.Types
import qualified Network.AWS.SecretsManager as SecretsManager
import Qtility
import Qtility.Database (createRDSConnectionPool, createRDSConnectionPool')
import Qtility.Database.Types

-- | Gets the secret value belonging to a 'SecretARN' and decodes it as an @a@. If the secret cannot
-- be found throws 'GetSecretNoSecretFound', if the value cannot be decoded correctly throws a
-- 'GetSecretDecodingError'. On AWS error, throws 'AWS.Error'. Reads the AWS environment value from
-- your environment.
getSecretValueAs ::
  (MonadUnliftIO m, MonadReader env m, MonadThrow m, AWS.HasEnv env, FromJSON a) =>
  SecretARN ->
  m a
getSecretValueAs secretArn = do
  awsEnv <- view AWS.environment
  getSecretValueAs' awsEnv secretArn

-- | Gets the secret value belonging to a 'SecretARN' as a @Text@. If the secret cannot be found
-- throws 'GetSecretNoSecretFound'. On AWS error, throws 'AWS.Error'. Reads the AWS environment
-- value from your environment.
getSecretValue ::
  (MonadUnliftIO m, MonadReader env m, MonadThrow m, AWS.HasEnv env) =>
  SecretARN ->
  m SecretValue
getSecretValue secretArn = do
  awsEnv <- view AWS.environment
  getSecretValue' awsEnv secretArn

-- | Gets the secret value belonging to a 'SecretARN' and decodes it as an @a@. If the secret cannot
-- be found throws 'GetSecretNoSecretFound', if the value cannot be decoded correctly throws a
-- 'GetSecretDecodingError'. On AWS error, throws 'AWS.Error'. This is an a'la carte version of
-- 'getSecretValue' that takes the AWS environment as an argument.
getSecretValueAs' ::
  (MonadUnliftIO m, MonadThrow m, FromJSON a) =>
  AWS.Env ->
  SecretARN ->
  m a
getSecretValueAs' awsEnv secretArn = do
  secretValue <- getSecretValue' awsEnv secretArn
  secretValue
    & (^. unwrap)
    & encodeUtf8
    & eitherDecodeStrict'
    & mapLeft GetSecretDecodingError
    & fromEither

-- | Gets the secret value belonging to a 'SecretARN' as a @Text@. If no value is found, throws
-- a 'GetSecretNoSecretFound' error. On AWS error, throws 'AWS.Error'. This is an a'la carte version
-- of 'getSecretValue' that takes the AWS environment as an argument.
getSecretValue' :: (MonadUnliftIO m, MonadThrow m) => AWS.Env -> SecretARN -> m SecretValue
getSecretValue' awsEnv secretArn = do
  let command = SecretsManager.getSecretValue (secretArn ^. unwrap)
  fromMaybeM (GetSecretNoSecretFound secretArn) $
    ((^. SecretsManager.gsvrsSecretString) >>> fmap SecretValue) <$> runAWS' awsEnv command

-- | Create a @'Pool' 'Connection'@ for a given 'SecretARN'. The secret is automatically fetched and
-- decoded into an 'RDSSecret', then a pool is created using that information. If the secret cannot
-- be found, throws 'GetSecretNoSecretFound'. If the secret cannot be decoded a
-- 'GetSecretDecodingError' is thrown. On AWS error, throws 'AWS.Error'. This reads your AWS
-- environment from your reader environment.
createConnectionPoolForSecretArn ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env) =>
  DatabaseConnections ->
  SecretARN ->
  m (Pool Connection)
createConnectionPoolForSecretArn connections secretArn = do
  awsEnv <- view AWS.environment
  createConnectionPoolForSecretArn' awsEnv connections secretArn

-- | Create a @'Pool' 'Connection'@ for a given 'SecretARN'. The secret is automatically fetched and
-- decoded into an 'RDSSecret', then a pool is created using that information. If the secret cannot
-- be found, throws 'GetSecretNoSecretFound'. If the secret cannot be decoded a
-- 'GetSecretDecodingError' is thrown. On AWS error, throws 'AWS.Error'. This is an a'la carte
-- version that takes your AWS environment as an argument.
createConnectionPoolForSecretArn' ::
  (MonadUnliftIO m, MonadThrow m) =>
  AWS.Env ->
  DatabaseConnections ->
  SecretARN ->
  m (Pool Connection)
createConnectionPoolForSecretArn' awsEnv connections secretArn = do
  secret <- getSecretValueAs' awsEnv secretArn
  createRDSConnectionPool connections secret

-- | Create a @'Pool' 'Connection'@ for a given 'SecretARN' but provide a database name as a
-- fallback when the secret is an instance identifier object, which does not have a database name
-- in it. The secret is automatically fetched and decoded into an 'RDSSecret', then a pool is
-- created using that information. If the secret cannot be found, throws 'GetSecretNoSecretFound'.
-- If the secret cannot be decoded a 'GetSecretDecodingError' is thrown. On AWS error, throws
-- 'AWS.Error'. This reads your AWS environment from your reader environment.
createConnectionPoolForSecretArnWithDbName ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env) =>
  DatabaseName ->
  DatabaseConnections ->
  SecretARN ->
  m (Pool Connection)
createConnectionPoolForSecretArnWithDbName dbName connections secretArn = do
  awsEnv <- view AWS.environment
  createConnectionPoolForSecretArnWithDbName' awsEnv dbName connections secretArn

-- | Create a @'Pool' 'Connection'@ for a given 'SecretARN' but provide a database name as a
-- fallback when the secret is an instance identifier object, which does not have a database name
-- in it. The secret is automatically fetched and decoded into an 'RDSSecret', then a pool is
-- created using that information. If the secret cannot be found, throws 'GetSecretNoSecretFound'.
-- If the secret cannot be decoded a 'GetSecretDecodingError' is thrown. On AWS error, throws
-- 'AWS.Error'. This reads your AWS environment from your reader environment. This is an a'la carte
-- version that takes your AWS environment as an argument.
createConnectionPoolForSecretArnWithDbName' ::
  (MonadUnliftIO m, MonadThrow m) =>
  AWS.Env ->
  DatabaseName ->
  DatabaseConnections ->
  SecretARN ->
  m (Pool Connection)
createConnectionPoolForSecretArnWithDbName' awsEnv dbName connections secretArn = do
  secret <- getSecretValueAs' awsEnv secretArn
  createRDSConnectionPool' dbName connections secret
