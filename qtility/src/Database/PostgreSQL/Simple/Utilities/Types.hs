{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.Utilities.Types where

import Control.Lens.TH (makeLenses, makeWrapped)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import RIO

newtype DatabaseName = DatabaseName {unDatabaseName :: ByteString}
  deriving (Eq, Ord, Show, Read, IsString)

instance ToField DatabaseName where
  toField = unDatabaseName >>> EscapeIdentifier

newtype DatabaseSchema = DatabaseSchema {unDatabaseSchema :: Text}
  deriving (Eq, Ord, Show, Read, IsString)

instance ToField DatabaseSchema where
  toField = unDatabaseSchema >>> encodeUtf8 >>> EscapeIdentifier

newtype DatabaseOwner = DatabaseOwner {unDatabaseOwner :: ByteString}
  deriving (Eq, Ord, Show, Read, IsString)

instance ToField DatabaseOwner where
  toField = unDatabaseOwner >>> EscapeIdentifier

newtype DatabaseTable = DatabaseTable {unDatabaseTable :: ByteString}
  deriving (Eq, Ord, Show, Read, IsString)

instance ToField DatabaseTable where
  toField = unDatabaseTable >>> EscapeIdentifier

newtype DatabaseConnections = DatabaseConnections {unDatabaseConnections :: Int}
  deriving (Eq, Show, Read, Ord, Num)

data RDSSecret = RDSSecret
  { _rdsSecretClusterIdentifier :: !Text,
    _rdsSecretName :: !Text,
    _rdsSecretEngine :: !Text,
    _rdsSecretHost :: !Text,
    _rdsSecretPassword :: !Text,
    _rdsSecretPort :: !Int,
    _rdsSecretUsername :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON RDSSecret where
  parseJSON = withObject "RDSSecret" $ \o ->
    RDSSecret
      <$> o .: "dbClusterIdentifier"
      <*> o .: "dbname"
      <*> o .: "engine"
      <*> o .: "host"
      <*> o .: "password"
      <*> o .: "port"
      <*> o .: "username"

foldMapM
  makeWrapped
  [ ''DatabaseName,
    ''DatabaseOwner,
    ''DatabaseConnections,
    ''DatabaseSchema,
    ''DatabaseTable
  ]

foldMapM makeLenses [''RDSSecret]
