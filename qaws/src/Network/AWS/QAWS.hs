module Network.AWS.QAWS where

import qualified Network.AWS as AWS
import qualified Network.AWS.Auth as AWS
import Qtility.Environment (loadDotEnvFile)
import Qtility.Environment.Types
import RIO

data LoadEnvironmentError
  = LoadEnvironmentNotFoundError EnvironmentFileNotFound
  | LoadEnvironmentAWSAuthError AWS.AuthError
  deriving (Show)

instance Exception LoadEnvironmentError

loadAWSEnvironment :: EnvironmentFile -> IO (Either LoadEnvironmentError AWS.Env)
loadAWSEnvironment environmentFile = do
  loadEnvResult <- mapLeft LoadEnvironmentNotFoundError <$> loadDotEnvFile environmentFile
  case loadEnvResult of
    Left e -> pure $ Left e
    Right () -> do
      envResult <- mapLeft LoadEnvironmentAWSAuthError <$> try (AWS.newEnv AWS.Discover)
      case envResult of
        err@(Left _) -> pure err
        Right e -> pure $ Right e

tryRunAWS ::
  (MonadReader env m, AWS.AWSRequest a, AWS.HasEnv env, MonadUnliftIO m) =>
  a ->
  m (Either AWS.Error (AWS.Rs a))
tryRunAWS = runAWS >>> try

runAWS ::
  (MonadReader env m, AWS.AWSRequest a, AWS.HasEnv env, MonadUnliftIO m) =>
  a ->
  m (AWS.Rs a)
runAWS action = do
  awsEnvironment <- view AWS.environment
  AWS.runResourceT $ AWS.runAWS awsEnvironment $ AWS.send action

tryRunAWS' ::
  (AWS.AWSRequest a, MonadUnliftIO m) =>
  AWS.Env ->
  a ->
  m (Either AWS.Error (AWS.Rs a))
tryRunAWS' env = runAWS' env >>> try

runAWS' ::
  (AWS.AWSRequest a, MonadUnliftIO m) =>
  AWS.Env ->
  a ->
  m (AWS.Rs a)
runAWS' env action = AWS.runResourceT $ AWS.runAWS env $ AWS.send action
