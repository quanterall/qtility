{-# LANGUAGE QuasiQuotes #-}

-- | A module for 'DB' queries that manipulate migrations. Most of them take a
-- 'Maybe DatabaseSchema' which will determine whether or not we prefix the migration table with a
-- schema or not.
module Qtility.Database.Migration.Queries where

import Database.PostgreSQL.Simple (execute, execute_, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Qtility.Data (unwrap)
import Qtility.Database (DB, HasPostgreSQLConnection (..))
import Qtility.Database.Types
import Qtility.Types (PositiveInteger)
import RIO
import qualified RIO.Text as Text

-- | Creates a migration table to hold applied and unapplied migrations. As the name suggests, this
-- function is safe to call multiple times as it will do nothing if the table already exists.
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

-- | Inserts the given migration in the migration table. Migrations are keyed on their filename and
-- when one already exists it won't be updated.
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

-- Removes all migrations from the migration table.
removeAllMigrations :: Maybe DatabaseSchema -> DB ()
removeAllMigrations maybeSchema = do
  connection <- view postgreSQLConnectionL
  void $
    liftIO $
      execute
        connection
        [sql|
          TRUNCATE TABLE ? CASCADE;
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
        ORDER BY filename ASC
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
        ORDER BY filename ASC
      |]
      (Only $ migrationTableName maybeSchema)

-- | Updates the information of a migration, keyed on the filename. If there are no applied
-- migrations this throws a 'NoMigrationsFound' exception.
rollbackLastNMigrations :: Maybe DatabaseSchema -> PositiveInteger -> DB ()
rollbackLastNMigrations maybeSchema n = do
  connection <- view postgreSQLConnectionL
  appliedMigrations <-
    liftIO $
      query
        connection
        [sql|
          SELECT filename, up_statement, down_statement, timestamp, is_applied
          FROM ?
          WHERE is_applied = true
          ORDER BY filename DESC LIMIT ?
        |]
        (migrationTableName maybeSchema, n)
  case appliedMigrations of
    [] -> throwM $ NoMigrationsFound $ migrationTableName maybeSchema
    migrations -> do
      forM_ migrations $ \migration -> do
        void $
          liftIO $
            execute_ connection (migration ^. migrationDownStatement & Text.unpack & fromString)
        void $
          liftIO $
            execute
              connection
              [sql|UPDATE ? SET is_applied = false WHERE filename = ?|]
              (migrationTableName maybeSchema, migration ^. migrationFilename)

-- | Updates the information of a migration, keyed on the filename. If the migration cannot be found
-- this throws a 'MigrationNotFound' exception.
updateMigration :: Maybe DatabaseSchema -> Migration -> DB ()
updateMigration maybeSchema migration = do
  connection <- view postgreSQLConnectionL
  affectedRows <-
    liftIO $
      execute
        connection
        [sql|
          UPDATE ? SET up_statement = ?, down_statement = ?, timestamp = ?, is_applied = ?
          WHERE filename = ?;
        |]
        ( migrationTableName maybeSchema,
          migration ^. migrationUpStatement,
          migration ^. migrationDownStatement,
          migration ^. migrationTimestamp,
          migration ^. migrationIsApplied,
          migration ^. migrationFilename
        )
  when (affectedRows == 0) $ throwM $ MigrationNotFound $ migration ^. migrationFilename

-- | Removes a migration from the migration table (not disk). If the migration cannot be found this
-- throws a 'MigrationNotFound' exception.
removeMigration :: Maybe DatabaseSchema -> FilePath -> DB ()
removeMigration maybeSchema filename = do
  connection <- view postgreSQLConnectionL
  affectedRows <-
    liftIO $
      execute
        connection
        [sql|
          DELETE FROM ? WHERE filename = ?;
        |]
        (migrationTableName maybeSchema, filename)
  when (affectedRows == 0) $ throwM $ MigrationNotFound filename

migrationTableName :: Maybe DatabaseSchema -> QualifiedIdentifier
migrationTableName maybeSchema = QualifiedIdentifier (fmap (^. unwrap) maybeSchema) "migrations"
