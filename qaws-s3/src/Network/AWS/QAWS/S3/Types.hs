{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.S3.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Qtility.TH.Lens (deriveWrappeds)
import RIO

newtype KeyPrefix = KeyPrefix {_unKeyPrefix :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype StartAfter = StartAfter {_unStartAfter :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype ContinuationToken = ContinuationToken {_unContinuationToken :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MaxKeys = MaxKeys {_unMaxKeys :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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

foldMapM
  makeLenses
  [ ''ListObjectOptions,
    ''MaxKeys,
    ''ContinuationToken,
    ''KeyPrefix,
    ''StartAfter
  ]

deriveWrappeds [''KeyPrefix, ''StartAfter, ''ContinuationToken, ''MaxKeys]
