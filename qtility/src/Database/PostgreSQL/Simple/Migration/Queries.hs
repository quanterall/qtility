{-# LANGUAGE QuasiQuotes #-}

module Database.PostgreSQL.Simple.Migration.Queries where

import Database.PostgreSQL.Simple (execute, query)
import Database.PostgreSQL.Simple.Migration.Types
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Utilities (DB, HasPostgreSQLConnection (..))
import Database.PostgreSQL.Simple.Utilities.Types
import Qtility.Data (unwrap)
import RIO

createMigrationTableIfNotExists :: Maybe DatabaseSchema -> DB ()
createMigrationTableIfNotExists maybeSchema = do
  connection <- view postgreSQLConnectionL
  void $
    liftIO $
      execute
        connection
        [sql|
          CREATE TABLE IF NOT EXISTS ? (
            filename TEXT UNIQUE NOT NULL,
            up_statement TEXT NOT NULL,
            down_statement TEXT NOT NULL,
            timestamp TIMESTAMPTZ NOT NULL,
            is_applied BOOLEAN NOT NULL
          );
          |]
        (Only $ migrationTableName maybeSchema)

insertMigrations :: Maybe DatabaseSchema -> [Migration] -> DB ()
insertMigrations maybeSchema migrations = do
  connection <- view postgreSQLConnectionL
  forM_ migrations $ \migration -> do
    liftIO $
      execute
        connection
        [sql|
          INSERT INTO ? (filename, up_statement, down_statement, timestamp, is_applied)
          VALUES (?, ?, ?, ?, ?)
          ON CONFLICT (filename) DO NOTHING;
       |]
        (TableAndMigration (migrationTableName maybeSchema) migration)

getMigrations :: Maybe DatabaseSchema -> DB [Migration]
getMigrations maybeSchema = do
  connection <- view postgreSQLConnectionL
  liftIO $
    query
      connection
      [sql|
        SELECT filename, up_statement, down_statement, timestamp, is_applied
        FROM ?
        ORDER BY filename ASC;
      |]
      (Only $ migrationTableName maybeSchema)

removeAllMigrations :: Maybe DatabaseSchema -> DB ()
removeAllMigrations maybeSchema = do
  connection <- view postgreSQLConnectionL
  void $
    liftIO $
      execute
        connection
        [sql|
          TRUNCATE TABLE ? RESTART IDENTITY;
        |]
        (Only $ migrationTableName maybeSchema)

migrationTableName :: Maybe DatabaseSchema -> QualifiedIdentifier
migrationTableName maybeSchema = QualifiedIdentifier (fmap (^. unwrap) maybeSchema) "migrations"
