{-# LANGUAGE TemplateHaskell #-}

module Qtility.Types where

import Control.Lens.TH (makeWrapped)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import RIO

newtype PositiveInteger = PositiveInteger {unPositiveInteger :: Natural}
  deriving (Eq, Ord, Show, Read, Num, Enum, Integral, Real, Generic)

instance ToField PositiveInteger where
  toField (PositiveInteger n) = n & toInteger & toField

foldMapM makeWrapped [''PositiveInteger]
