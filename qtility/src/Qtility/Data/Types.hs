{-# LANGUAGE TemplateHaskell #-}

module Qtility.Data.Types where

import Control.Lens.TH (makeLenses)
import RIO

newtype Seconds a = Seconds {_unSeconds :: a}
  deriving (Eq, Show)

newtype Milliseconds a = Milliseconds {_unMilliseconds :: a}
  deriving (Eq, Show)

newtype Microseconds a = Microseconds {_unMicroseconds :: a}
  deriving (Eq, Show)

foldMapM makeLenses [''Seconds, ''Milliseconds, ''Microseconds]
