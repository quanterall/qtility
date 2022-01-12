{-# LANGUAGE TemplateHaskell #-}

module Qtility.Time.Types where

import Control.Lens.TH (makeLenses, makeWrapped)
import RIO
import RIO.Time (NominalDiffTime)

newtype Seconds a = Seconds {_unSeconds :: a}
  deriving (Eq, Show)

newtype Milliseconds a = Milliseconds {_unMilliseconds :: a}
  deriving (Eq, Show)

newtype Microseconds a = Microseconds {_unMicroseconds :: a}
  deriving (Eq, Show)

data TimedResult a = TimedResult
  { _trTime :: NominalDiffTime,
    _trValue :: a
  }
  deriving (Eq, Show, Generic)

foldMapM makeLenses [''Seconds, ''Milliseconds, ''Microseconds, ''TimedResult]

foldMapM makeWrapped [''Seconds, ''Milliseconds, ''Microseconds]
