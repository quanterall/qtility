{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SecretsManager.Types where

import Qtility
import Qtility.TH.Optics (makeClassyException)

newtype SecretARN = SecretARN {unSecretARN :: Text}
  deriving (Eq, Show, Read, Generic, Ord, IsString, FromJSON, ToJSON)

newtype SecretValue = SecretValue {unSecretValue :: Text}
  deriving (Eq, Show, Read, Generic, Ord, IsString, FromJSON, ToJSON)

data GetSecretError
  = GetSecretNoSecretFound !SecretARN
  | GetSecretDecodingError !String
  deriving (Eq, Show, Read, Generic)

foldMapM makeClassyException [''GetSecretError]

foldMapM makeWrapped [''SecretARN, ''SecretValue]
