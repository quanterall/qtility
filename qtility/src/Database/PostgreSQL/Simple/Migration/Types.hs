{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.Migration.Types where

import Control.Lens.TH (makeLenses)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Qtility.TH.Optics (makeClassyException)
import RIO
import RIO.Time (UTCTime)

data Migration = Migration
  { _migrationName :: !Text,
    _migrationUpStatement :: !Text,
    _migrationDownStatement :: !Text,
    _migrationFilename :: !FilePath,
    _migrationTimestamp :: !UTCTime,
    _migrationIsApplied :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToRow Migration where
  toRow (Migration name up down filename timestamp isApplied) =
    [toField name, toField up, toField down, toField filename, toField timestamp, toField isApplied]

data MigrationFileError
  = MigrationIncorrectFormat FilePath
  | MigrationIncorrectFilename FilePath
  deriving (Eq, Show, Generic)

foldMapM makeClassyException [''MigrationFileError]

foldMapM makeLenses [''Migration]
