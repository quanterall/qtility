{-# LANGUAGE NumericUnderscores #-}

-- | Has utility functions for dealing with Amazon's Security Token Service (STS).
module Network.AWS.QAWS.STS (assumeRoleWithWebIdentity) where

import qualified Network.AWS as AWS
import Network.AWS.QAWS
import Network.AWS.QAWS.STS.Types
import Network.AWS.QAWS.Types
import qualified Network.AWS.STS as AWSSTS
import Qtility
import Qtility.Time.Types (Seconds (..))

-- | Assumes a given role, useful for contexts where the role credentials are passed in to the
-- environment, like EKS.
assumeRoleWithWebIdentity ::
  (MonadUnliftIO m, MonadReader env m, HasLogFunc env) =>
  AWS.Env ->
  IamRoleArn ->
  WebIdentityToken ->
  SessionName ->
  CredentialsDuration ->
  m (AWS.Env, Async ())
assumeRoleWithWebIdentity
  awsEnv
  (IamRoleArn (Arn roleArn))
  (WebIdentityToken token)
  (SessionName sessionName)
  (CredentialsDuration (Seconds duration)) = do
    let command =
          AWSSTS.assumeRoleWithWebIdentity roleArn token sessionName
            & AWSSTS.arwwiDurationSeconds ?~ duration
    -- This holds the `IORef AuthEnv` we are waiting to use, which will be filled in by the thread
    -- we are starting here.
    ioRefVar <- newEmptyMVar
    asyncThread <- async $ initialNegotiation ioRefVar command
    authEnvRef <- readMVar ioRefVar
    pure (awsEnv & AWS.envAuth .~ AWS.Ref (asyncThreadId asyncThread) authEnvRef, asyncThread)
    where
      initialNegotiation ioRefVar command = do
        maybeAuthEnv <-
          ((^. AWSSTS.arwwirsCredentials) <$> runAWS' awsEnv command)
            `catchAny` \e -> do
              logErrorS "assumeRoleWithWebIdentity" $ "Error while trying to assume role: " <> displayShow e
              pure Nothing
        case maybeAuthEnv of
          Nothing -> do
            threadDelay 100_000
            initialNegotiation ioRefVar command
          Just authEnv -> do
            ref <- newIORef authEnv
            putMVar ioRefVar ref
            threadDelay $ (fromIntegral duration - 10) * 1_000_000
            refreshLoop ref command
      refreshLoop ref command = do
        response' <- runAWS' awsEnv command
        let maybeAuthEnv = response' ^. AWSSTS.arwwirsCredentials
        case maybeAuthEnv of
          Nothing -> do
            threadDelay 100_000
            refreshLoop ref command
          Just authEnv -> do
            atomicWriteIORef ref authEnv
            threadDelay $ (fromIntegral duration - 10) * 1_000_000
            refreshLoop ref command
