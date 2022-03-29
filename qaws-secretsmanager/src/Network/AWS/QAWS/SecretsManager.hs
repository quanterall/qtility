-- | Has utility functions for dealing with Amazon's Simple Storage Service (S3).
module Network.AWS.QAWS.SecretsManager
  ( getSecretValue,
    getSecretValue',
  )
where

import qualified Network.AWS as AWS
import Network.AWS.QAWS (runAWS')
import Network.AWS.QAWS.SecretsManager.Types
import qualified Network.AWS.SecretsManager as SecretsManager
import Qtility

getSecretValue ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  SecretARN ->
  m (Maybe SecretValue)
getSecretValue secretArn = do
  awsEnv <- view AWS.environment
  getSecretValue' awsEnv secretArn

getSecretValue' :: (MonadUnliftIO m) => AWS.Env -> SecretARN -> m (Maybe SecretValue)
getSecretValue' awsEnv secretArn = do
  let command = SecretsManager.getSecretValue (secretArn ^. unwrap)
  ((^. SecretsManager.gsvrsSecretString) >>> fmap SecretValue) <$> runAWS' awsEnv command
