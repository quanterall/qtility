{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.Migration.Types where

import Control.Lens.TH (makeLenses, makeWrapped)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
import Qtility.TH.Optics (makeClassyException)
import RIO
import RIO.Time (UTCTime)

newtype NoMigrationsFound = NoMigrationsFound {unNoMigrationsFound :: QualifiedIdentifier}
  deriving (Eq, Show, Generic)

instance Exception NoMigrationsFound

newtype MigrationNotFound = MigrationNotFound {unMigrationNotFound :: String}
  deriving (Eq, Show, Generic)

instance Exception MigrationNotFound

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

foldMapM makeClassyException [''MigrationFileError]

foldMapM makeLenses [''Migration]

foldMapM makeWrapped [''NoMigrationsFound, ''MigrationNotFound]
