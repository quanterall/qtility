{-# LANGUAGE TemplateHaskell #-}

module Qtility.Code.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, Options, ToJSON)
import Qtility.JSON (deriveJson, lowerCaseFirstCharacter, prefixedLensOptions)
import RIO

opts :: String -> Options
opts name = prefixedLensOptions name (drop 1 >>> lowerCaseFirstCharacter)

newtype ModuleName = ModuleName {_unModuleName :: String}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makeLenses ''ModuleName

data Import = Import
  { _importModule :: ModuleName,
    _importQualifiedName :: Maybe ModuleName
  }
  deriving (Eq, Show, Generic)

deriveJson ''Import
makeLenses ''Import
