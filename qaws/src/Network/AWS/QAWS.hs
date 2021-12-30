module Network.AWS.QAWS where

import Control.Monad.Catch (MonadCatch)
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

-- | Loads the available AWS environment in part with the help of the given 'EnvironmentFile'. The
-- environment file is read and injected into the process environment, and the values that make up
-- the needed AWS credentials are in part expected from there. The keys in question are
-- @AWS_ACCESS_KEY_ID@, @AWS_SECRET_ACCESS_KEY@ and @AWS_REGION@. If the reading of the file or the
-- loading of the AWS environment otherwise fails, a 'LoadEnvironmentError' is thrown.
loadAWSEnvironment :: (MonadIO m, MonadUnliftIO m, MonadCatch m) => EnvironmentFile -> m AWS.Env
loadAWSEnvironment environmentFile = do
  mapExceptionM LoadEnvironmentNotFoundError $ loadDotEnvFile environmentFile
  mapExceptionM LoadEnvironmentAWSAuthError $ AWS.newEnv AWS.Discover

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
