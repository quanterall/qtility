{-# LANGUAGE TemplateHaskell #-}

module Qtility.Data.Types where

import Control.Lens.TH (makeLenses)
import RIO

newtype Seconds a = Seconds {_unSeconds :: a}
  deriving (Eq, Show)

makeLenses ''Seconds

newtype Milliseconds a = Milliseconds {_unMilliseconds :: a}
  deriving (Eq, Show)

makeLenses ''Milliseconds

newtype Microseconds a = Microseconds {_unMicroseconds :: a}
  deriving (Eq, Show)

makeLenses ''Microseconds
