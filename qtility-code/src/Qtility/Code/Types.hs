{-# LANGUAGE TemplateHaskell #-}

module Qtility.Code.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Qtility.TH (deriveLensAndJSON)
import Qtility.TH.Lens (deriveWrappeds)
import RIO

newtype ModuleName = ModuleName {_unModuleName :: String}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makeLenses ''ModuleName

data Import = Import
  { _importModule :: !ModuleName,
    _importQualifiedName :: !(Maybe ModuleName)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Import

deriveWrappeds [''ModuleName]
