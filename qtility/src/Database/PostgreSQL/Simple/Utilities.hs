{-# LANGUAGE QuasiQuotes #-}

-- | Utilities for dealing with `postgresql-simple`. This includes support for running queries and
-- statements if one has a 'Pool Connection' available in the current environment.
module Database.PostgreSQL.Simple.Utilities where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Utilities.Types
import Qtility

class HasPostgresqlPool env where
  postgresqlPoolL :: Lens' env (Pool Connection)

instance HasPostgresqlPool (Pool Connection) where
  postgresqlPoolL = id

class HasPostgresqlMasterPool env where
  postgresqlMasterPoolL :: Lens' env (Pool Connection)

instance HasPostgresqlMasterPool (Pool Connection) where
  postgresqlMasterPoolL = id

-- | Runs an action against the connection pool that has been chosen for the main pool of the
-- application. This is different to 'runMasterDB', which (idiomatically) would run against the
-- @postgres@ database.
runDB ::
  (MonadReader env m, MonadUnliftIO m, HasPostgresqlPool env) =>
  (Connection -> IO a) ->
  m a
runDB action = do
  pool <- view postgresqlPoolL
  liftIO $ withResource pool action

-- | Runs an action against the master database of PostgreSQL, i.e. the @postgres@ database.
runMasterDB ::
  (MonadIO m, MonadReader env m, HasPostgresqlMasterPool env) =>
  (Connection -> IO a) ->
  m a
runMasterDB action = do
  pool <- view postgresqlMasterPoolL
  liftIO $ withResource pool action

createDatabaseIfNotExists ::
  (MonadIO m, MonadReader env m, HasPostgresqlMasterPool env) =>
  DatabaseName ->
  DatabaseOwner ->
  m ()
createDatabaseIfNotExists name owner = do
  runMasterDB $ \connection -> do
    unlessM (doesDatabaseExist name connection) $ do
      void $ execute connection [sql|CREATE DATABASE ? WITH OWNER ?|] (name, owner)

doesDatabaseExist :: DatabaseName -> Connection -> IO Bool
doesDatabaseExist (DatabaseName name) connection = do
  (length @[] @[Text] >>> (> 0))
    <$> query
      connection
      [sql|SELECT datname FROM pg_catalog.pg_database WHERE datname = ?|]
      (Only name)
