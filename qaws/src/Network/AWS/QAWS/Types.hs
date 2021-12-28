{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import RIO

newtype ARN = ARN {_unARN :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''ARN
