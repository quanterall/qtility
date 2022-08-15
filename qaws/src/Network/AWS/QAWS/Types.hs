{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.Types where

import Control.Lens.TH (makeWrapped)
import Data.Aeson (FromJSON, ToJSON)
import qualified Network.AWS.Auth as AWS
import Qtility.Environment (FromEnvironmentValue)
import Qtility.Environment.Types
import Qtility.TH.Optics (makeClassyException)
import RIO

newtype Arn = Arn {unArn :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromEnvironmentValue)

data LoadEnvironmentError
  = LoadEnvironmentNotFoundError EnvironmentFileNotFound
  | LoadEnvironmentAWSAuthError AWS.AuthError
  deriving (Show)

foldMapM makeWrapped [''Arn]

foldMapM makeClassyException [''LoadEnvironmentError]
