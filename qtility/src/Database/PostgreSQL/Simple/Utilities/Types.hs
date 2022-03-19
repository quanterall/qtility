{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.Utilities.Types where

import Control.Lens.TH (makeWrapped)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import RIO

newtype DatabaseName = DatabaseName {unDatabaseName :: ByteString}
  deriving (Eq, Ord, Show, Read, IsString)

instance ToField DatabaseName where
  toField = unDatabaseName >>> EscapeIdentifier

newtype DatabaseOwner = DatabaseOwner {unDatabaseOwner :: ByteString}
  deriving (Eq, Ord, Show, Read, IsString)

instance ToField DatabaseOwner where
  toField = unDatabaseOwner >>> EscapeIdentifier

foldMapM makeWrapped [''DatabaseName, ''DatabaseOwner]
