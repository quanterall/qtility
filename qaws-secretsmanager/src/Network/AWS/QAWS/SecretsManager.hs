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

-- | Gets the secret value belonging to a 'SecretARN' and decodes it as an @a@. If the value cannot
-- be decoded correctly this throws a 'GetSecretDecodingError'. On AWS error, throws 'AWS.Error'.
-- Reads the AWS environment value from your environment.
getSecretValueAs ::
  (MonadUnliftIO m, MonadReader env m, MonadThrow m, AWS.HasEnv env, FromJSON a) =>
  SecretARN ->
  m (Maybe a)
getSecretValueAs secretArn = do
  awsEnv <- view AWS.environment
  getSecretValueAs' awsEnv secretArn

-- | Gets the secret value belonging to a 'SecretARN' as a @Text@. On AWS error, throws 'AWS.Error'.
-- Reads the AWS environment value from your environment.
getSecretValue ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  SecretARN ->
  m (Maybe SecretValue)
getSecretValue secretArn = do
  awsEnv <- view AWS.environment
  getSecretValue' awsEnv secretArn

-- | Gets the secret value belonging to a 'SecretARN' and decodes it as an @a@. If the value cannot
-- be decoded correctly this throws a 'GetSecretDecodingError'. On AWS error, throws 'AWS.Error'.
-- This is an a'la carte version of 'getSecretValue' that takes the AWS environment as an argument.
getSecretValueAs' ::
  (MonadUnliftIO m, MonadThrow m, FromJSON a) =>
  AWS.Env ->
  SecretARN ->
  m (Maybe a)
getSecretValueAs' awsEnv secretArn = do
  maybeSecretValue <- getSecretValue' awsEnv secretArn
  forM
    maybeSecretValue
    ( (^. unwrap)
        >>> encodeUtf8
        >>> eitherDecodeStrict'
        >>> mapLeft GetSecretDecodingError
        >>> fromEither
    )

-- | Gets the secret value belonging to a 'SecretARN' as a @Text@. On AWS error, throws 'AWS.Error'.
-- This is an a'la carte version of 'getSecretValue' that takes the AWS environment as an argument.
getSecretValue' :: (MonadUnliftIO m) => AWS.Env -> SecretARN -> m (Maybe SecretValue)
getSecretValue' awsEnv secretArn = do
  let command = SecretsManager.getSecretValue (secretArn ^. unwrap)
  ((^. SecretsManager.gsvrsSecretString) >>> fmap SecretValue) <$> runAWS' awsEnv command
