{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.S3.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import RIO

newtype KeyPrefix = KeyPrefix {_unKeyPrefix :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''KeyPrefix

newtype StartAfter = StartAfter {_unStartAfter :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''StartAfter

newtype ContinuationToken = ContinuationToken {_unContinuationToken :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''ContinuationToken

newtype MaxKeys = MaxKeys {_unMaxKeys :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''MaxKeys

data ListObjectOptions = ListObjectOptions
  { _looKeyPrefix :: Maybe KeyPrefix,
    _looStartAfter :: Maybe StartAfter,
    _looContinuationToken :: Maybe ContinuationToken,
    _looMaxKeys :: Maybe MaxKeys
  }
  deriving (Eq, Show, Generic)

instance Default ListObjectOptions where
  def =
    ListObjectOptions
      { _looKeyPrefix = Nothing,
        _looStartAfter = Nothing,
        _looContinuationToken = Nothing,
        _looMaxKeys = Nothing
      }

makeLenses ''ListObjectOptions
