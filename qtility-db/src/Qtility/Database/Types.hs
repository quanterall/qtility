{-# LANGUAGE TemplateHaskell #-}

module Qtility.Database.Types where

import Control.Lens.TH (makeClassyPrisms, makeLenses, makeWrapped)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..), toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier, Query)
import Qtility.TH (deriveLensAndAbbreviatedJSON)
import Qtility.TH.Optics (makeClassyException)
import RIO
import RIO.Time (UTCTime)

-- | The database returned too many results for the given query.
newtype DBTooManyResults = DBTooManyResults {unDBTooManyResults :: Query}
  deriving (Eq, Show)

instance Exception DBTooManyResults

-- | The database returned no results when results were expected.
newtype DBNoResults = DBNoResults {unDBNoResults :: Query}
  deriving (Eq, Show)

instance Exception DBNoResults

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

data RDSSecret
  = RDSClusterInfo ClusterInstanceInfo
  | RDSInstanceInfo DatabaseInstanceInfo
  deriving (Eq, Show, Generic)

instance FromJSON RDSSecret where
  parseJSON v = RDSClusterInfo <$> parseJSON v <|> RDSInstanceInfo <$> parseJSON v

data ClusterInstanceInfo = ClusterInstanceInfo
  { _ciiClusterIdentifier :: !Text,
    _ciiName :: !Text,
    _ciiEngine :: !Text,
    _ciiHost :: !Text,
    _ciiPassword :: !Text,
    _ciiPort :: !Int,
    _ciiUsername :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ClusterInstanceInfo where
  parseJSON = withObject "RDSSecret" $ \o ->
    ClusterInstanceInfo
      <$> o .: "dbClusterIdentifier"
      <*> o .: "dbname"
      <*> o .: "engine"
      <*> o .: "host"
      <*> o .: "password"
      <*> o .: "port"
      <*> o .: "username"

data DatabaseInstanceInfo = DatabaseInstanceInfo
  { _diidbInstanceIdentifier :: !Text,
    _diiEngine :: !Text,
    _diiHost :: !Text,
    _diiPort :: !Int,
    _diiResourceId :: !Text,
    _diiUsername :: !Text,
    _diiPassword :: !Text
  }
  deriving (Eq, Show, Generic)

newtype NoMigrationsFound = NoMigrationsFound {unNoMigrationsFound :: QualifiedIdentifier}
  deriving (Eq, Show, Generic)

instance Exception NoMigrationsFound

newtype MigrationNotFound = MigrationNotFound {unMigrationNotFound :: String}
  deriving (Eq, Show, Generic)

instance Exception MigrationNotFound

newtype TooManyMigrations = TooManyMigrations {unTooManyMigrations :: String}
  deriving (Eq, Show, Generic)

instance Exception TooManyMigrations

data TableAndMigration = TableAndMigration
  { _tableAndMigrationTableName :: !QualifiedIdentifier,
    _tableAndMigrationMigration :: !Migration
  }

instance ToRow TableAndMigration where
  toRow (TableAndMigration tableName migration) =
    [toField tableName] <> toRow migration

data Migration = Migration
  { _migrationFilename :: !FilePath,
    _migrationUpStatement :: !Text,
    _migrationDownStatement :: !Text,
    _migrationTimestamp :: !UTCTime,
    _migrationIsApplied :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToRow Migration where
  toRow (Migration filename up down timestamp isApplied) =
    [toField filename, toField up, toField down, toField timestamp, toField isApplied]

instance FromRow Migration where
  fromRow = Migration <$> field <*> field <*> field <*> field <*> field

data MigrationFileError
  = MigrationIncorrectFormat FilePath
  | MigrationIncorrectFilename FilePath
  deriving (Eq, Show, Generic)

newtype PositiveInteger = PositiveInteger {unPositiveInteger :: Natural}
  deriving (Eq, Ord, Show, Read, Num, Enum, Integral, Real, Generic)

instance ToField PositiveInteger where
  toField (PositiveInteger n) = n & toInteger & toField

foldMapM makeWrapped [''PositiveInteger]

foldMapM makeClassyException [''MigrationFileError]

foldMapM
  makeClassyPrisms
  [ ''DBTooManyResults,
    ''DBNoResults,
    ''NoMigrationsFound,
    ''MigrationNotFound,
    ''TooManyMigrations
  ]

foldMapM makeLenses [''Migration]

foldMapM makeWrapped [''DBTooManyResults, ''DBNoResults, ''NoMigrationsFound, ''MigrationNotFound]

foldMapM
  makeWrapped
  [ ''DatabaseName,
    ''DatabaseOwner,
    ''DatabaseConnections,
    ''DatabaseSchema,
    ''DatabaseTable
  ]

foldMapM makeLenses [''ClusterInstanceInfo]

foldMapM deriveLensAndAbbreviatedJSON [''DatabaseInstanceInfo]
