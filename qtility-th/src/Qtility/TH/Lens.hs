{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Qtility.TH.Lens where

import Control.Lens.Wrapped (Wrapped)
import Language.Haskell.TH
import RIO

deriveWrapped :: Name -> Q [Dec]
deriveWrapped name = do
  [d|
    instance Wrapped $(conT name)
    |]

deriveWrappeds :: [Name] -> Q [Dec]
deriveWrappeds = foldMapM deriveWrapped
