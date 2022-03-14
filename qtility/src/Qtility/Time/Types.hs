{-# LANGUAGE TemplateHaskell #-}

module Qtility.Time.Types where

import Control.Lens.TH (makeLenses, makeWrapped)
import Data.Aeson (FromJSON, ToJSON)
import RIO
import RIO.Time (NominalDiffTime)

newtype Seconds a = Seconds {_unSeconds :: a}
  deriving (Eq, Show, FromJSON, ToJSON)

newtype Milliseconds a = Milliseconds {_unMilliseconds :: a}
  deriving (Eq, Show, FromJSON, ToJSON)

newtype Microseconds a = Microseconds {_unMicroseconds :: a}
  deriving (Eq, Show, FromJSON, ToJSON)

data TimedResult a = TimedResult
  { _trTime :: NominalDiffTime,
    _trValue :: a
  }
  deriving (Eq, Show, Generic)

newtype Timestamp = Timestamp {unTimestamp :: Milliseconds Integer}
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

foldMapM makeLenses [''Seconds, ''Milliseconds, ''Microseconds, ''TimedResult]

foldMapM makeWrapped [''Seconds, ''Milliseconds, ''Microseconds, ''Timestamp]
