-- | Has utility functions for dealing with Amazon's SecretsManager.
module Network.AWS.QAWS.SecretsManager
  ( getSecretValueAs,
    getSecretValueAs',
    getSecretValue,
    getSecretValue',
  )
where

import qualified Network.AWS as AWS
import Network.AWS.QAWS (runAWS')
import Network.AWS.QAWS.SecretsManager.Types
import qualified Network.AWS.SecretsManager as SecretsManager
import Qtility

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
