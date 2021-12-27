{-# LANGUAGE TemplateHaskell #-}

module Qtility.Environment.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import RIO

newtype EnvironmentKey = EnvironmentKey {_unEnvironmentKey :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''EnvironmentKey

newtype EnvironmentValue = EnvironmentValue {_unEnvironmentValue :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''EnvironmentValue

data LoadEnvironmentVariableError
  = LoadEnvironmentInvalidValue EnvironmentKey EnvironmentValue String
  | LoadEnvironmentMissingValue EnvironmentKey
  deriving (Eq, Show)

instance Exception LoadEnvironmentVariableError

newtype EnvironmentFile = EnvironmentFile {_unEnvironmentFile :: FilePath}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''EnvironmentFile

newtype EnvironmentFileNotFound = EnvironmentFileNotFound
  {_unEnvironmentFileNotFound :: EnvironmentFile}
  deriving (Eq, Show)

makeLenses ''EnvironmentFileNotFound
