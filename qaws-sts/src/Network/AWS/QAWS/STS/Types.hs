{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.STS.Types where

import Control.Lens.TH (makeLenses, makeWrapped)
import Data.Aeson (FromJSON (..), ToJSON (..))
-- import qualified Network.AWS as AWS
import Network.AWS.QAWS.Types (Arn)
import Qtility.Environment (FromEnvironmentValue (..))
import Qtility.TH.Optics (makeClassyException)
import Qtility.Time.Types (Seconds)
import RIO

data NoAuthEnvForAssumeRoleWithWebIdentity = NoAuthEnvForAssumeRoleWithWebIdentity
  deriving (Show, Eq, Generic)

instance Exception NoAuthEnvForAssumeRoleWithWebIdentity

newtype IamRoleArn = IamRoleArn {unIamRoleArn :: Arn}
  deriving (Eq, Show, FromEnvironmentValue, FromJSON, ToJSON)

newtype WebIdentityToken = WebIdentityToken {unWebIdentityToken :: Text}
  deriving (Eq, Show, FromEnvironmentValue, FromJSON, ToJSON)

newtype SessionName = SessionName {unSessionName :: Text}
  deriving (Eq, Show, FromEnvironmentValue, FromJSON, ToJSON)

newtype CredentialsDuration = CredentialsDuration {unCredentialsDuration :: Seconds Natural}
  deriving (Eq, Show, FromEnvironmentValue, FromJSON, ToJSON)

data AuthenticationThread = AuthenticationThread
  { _authenticationThreadIamRoleArn :: IamRoleArn,
    _authenticationThreadWebIdentityToken :: WebIdentityToken,
    _authenticationThreadSessionName :: SessionName,
    _authenticationThreadCredentialsDuration :: CredentialsDuration,
    _authenticationThreadThreadId :: ThreadId
  }
  deriving (Generic)

foldMapM makeWrapped [''IamRoleArn]

foldMapM makeLenses [''AuthenticationThread]

foldMapM makeClassyException []
