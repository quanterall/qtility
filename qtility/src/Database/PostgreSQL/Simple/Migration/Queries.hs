{-# LANGUAGE QuasiQuotes #-}

module Database.PostgreSQL.Simple.Migration.Queries where

import Database.PostgreSQL.Simple (execute, execute_, query)
import Database.PostgreSQL.Simple.Migration.Types
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Utilities (DB, HasPostgreSQLConnection (..))
import Database.PostgreSQL.Simple.Utilities.Types
import Qtility.Data (unwrap)
import RIO
import qualified RIO.Text as Text

createMigrationTableIfNotExists :: Maybe DatabaseSchema -> DB ()
createMigrationTableIfNotExists maybeSchema = do
  connection <- view postgreSQLConnectionL
  void $
    liftIO $
      execute
        connection
        [sql|
          SET LOCAL client_min_messages = warning;
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

applyMigrations :: Maybe DatabaseSchema -> [Migration] -> DB ()
applyMigrations maybeSchema migrations = do
  connection <- view postgreSQLConnectionL
  forM_ migrations $ \migration -> do
    unless (migration ^. migrationIsApplied) $ do
      void $
        liftIO $ do
          _ <- execute_ connection (migration ^. migrationUpStatement & Text.unpack & fromString)
          execute
            connection
            [sql|
            UPDATE ? SET is_applied = true WHERE filename = ?;
          |]
            (migrationTableName maybeSchema, migration ^. migrationFilename)

getAppliedMigrations :: Maybe DatabaseSchema -> DB [Migration]
getAppliedMigrations maybeSchema = do
  connection <- view postgreSQLConnectionL
  liftIO $
    query
      connection
      [sql|
        SELECT filename, up_statement, down_statement, timestamp, is_applied
        FROM ?
        WHERE is_applied = true
        ORDER BY filename ASC;
      |]
      (Only $ migrationTableName maybeSchema)

getUnappliedMigrations :: Maybe DatabaseSchema -> DB [Migration]
getUnappliedMigrations maybeSchema = do
  connection <- view postgreSQLConnectionL
  liftIO $
    query
      connection
      [sql|
        SELECT filename, up_statement, down_statement, timestamp, is_applied
        FROM ?
        WHERE is_applied = false
        ORDER BY filename ASC;
      |]
      (Only $ migrationTableName maybeSchema)

migrationTableName :: Maybe DatabaseSchema -> QualifiedIdentifier
migrationTableName maybeSchema = QualifiedIdentifier (fmap (^. unwrap) maybeSchema) "migrations"
