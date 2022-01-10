{-# LANGUAGE TemplateHaskell #-}

module Qtility.Environment.Types where

import Control.Lens.TH (makeClassyPrisms, makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Qtility.TH.Optics (makeClassyException)
import RIO

newtype EnvironmentKey = EnvironmentKey {_unEnvironmentKey :: String}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype EnvironmentValue = EnvironmentValue {_unEnvironmentValue :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReadEnvironmentVariableError
  = ReadEnvironmentInvalidValue !EnvironmentKey !EnvironmentValue !String
  | ReadEnvironmentMissingValue !EnvironmentKey
  deriving (Eq, Show)

newtype EnvironmentFile = EnvironmentFile {_unEnvironmentFile :: FilePath}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype EnvironmentFileNotFound = EnvironmentFileNotFound
  {_unEnvironmentFileNotFound :: EnvironmentFile}
  deriving (Eq, Show)

instance Exception EnvironmentFileNotFound

foldMapM
  makeLenses
  [ ''EnvironmentKey,
    ''EnvironmentValue,
    ''EnvironmentFile,
    ''EnvironmentFileNotFound
  ]

foldMapM makeClassyPrisms [''EnvironmentFileNotFound]

foldMapM makeClassyException [''ReadEnvironmentVariableError]
