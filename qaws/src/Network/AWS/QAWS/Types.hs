{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import qualified Network.AWS.Auth as AWS
import Qtility.Environment.Types
import Qtility.TH.Optics (makeClassyException)
import RIO

newtype ARN = ARN {_unARN :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data LoadEnvironmentError
  = LoadEnvironmentNotFoundError EnvironmentFileNotFound
  | LoadEnvironmentAWSAuthError AWS.AuthError
  deriving (Show)

foldMapM makeLenses [''ARN]

foldMapM makeClassyException [''LoadEnvironmentError]
