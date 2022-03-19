{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.Utilities.Types where

import Control.Lens.TH (makeWrapped)
import RIO

newtype DatabaseName = DatabaseName {unDatabaseName :: Text}
  deriving (Eq, Ord, Show, Read, IsString)

newtype DatabaseOwner = DatabaseOwner {unDatabaseOwner :: Text}
  deriving (Eq, Ord, Show, Read, IsString)

foldMapM makeWrapped [''DatabaseName, ''DatabaseOwner]
