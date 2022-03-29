{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SecretsManager.Types where

import Qtility

newtype SecretARN = SecretARN {unSecretARN :: Text}
  deriving (Eq, Ord, Show, Read, Generic)

newtype SecretValue = SecretValue {unSecretValue :: Text}
  deriving (Eq, Ord, Show, Read, Generic)

newtype GetSecretDecodingError = GetSecretDecodingError {unGetSecretDecodingError :: String}
  deriving (Eq, Ord, Show, Read, Generic)

instance Exception GetSecretDecodingError

foldMapM makeWrapped [''SecretARN, ''SecretValue, ''GetSecretDecodingError]
