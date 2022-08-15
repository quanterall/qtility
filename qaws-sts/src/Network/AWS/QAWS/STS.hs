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
  (MonadUnliftIO m) =>
  AWS.Env ->
  IamRoleArn ->
  WebIdentityToken ->
  SessionName ->
  CredentialsDuration ->
  m (AWS.Env, ThreadId)
assumeRoleWithWebIdentity
  awsEnv
  (IamRoleArn (Arn roleArn))
  (WebIdentityToken token)
  (SessionName sessionName)
  (CredentialsDuration (Seconds duration)) = do
    let command =
          AWSSTS.assumeRoleWithWebIdentity roleArn token sessionName
            & AWSSTS.arwwiDurationSeconds ?~ duration
    authEnvRef <- newIORef undefined
    waitVar <- newMVar ()
    threadId <- asyncThreadId <$> async (authenticationLoop True waitVar authEnvRef command)
    readMVar waitVar
    pure (awsEnv & AWS.envAuth .~ AWS.Ref threadId authEnvRef, threadId)
    where
      authenticationLoop firstRun waitVar ref command' = do
        response' <- runAWS' awsEnv command'
        let maybeAuthEnv' = response' ^. AWSSTS.arwwirsCredentials
        case maybeAuthEnv' of
          Nothing -> do
            threadDelay 100_000
            authenticationLoop firstRun waitVar ref command'
          Just authEnv -> do
            atomicWriteIORef ref authEnv
            when firstRun $ putMVar waitVar ()
            threadDelay $ (fromIntegral duration - 10) * 1_000_000
            authenticationLoop False waitVar ref command'
