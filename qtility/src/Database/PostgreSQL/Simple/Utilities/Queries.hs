{-# LANGUAGE QuasiQuotes #-}

module Database.PostgreSQL.Simple.Utilities.Queries where

import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Utilities
import Database.PostgreSQL.Simple.Utilities.Types
import RIO

createDatabaseIfNotExists :: DatabaseName -> DatabaseOwner -> DB ()
createDatabaseIfNotExists name owner = do
  connection <- view postgreSQLConnectionL
  unlessM (doesDatabaseExist name) $ do
    void $ liftIO $ execute connection [sql|CREATE DATABASE ? WITH OWNER ?|] (name, owner)

createDatabase :: DatabaseName -> DatabaseOwner -> DB ()
createDatabase name owner = do
  connection <- view postgreSQLConnectionL
  void $ liftIO $ execute connection [sql|CREATE DATABASE ? WITH OWNER ?|] (name, owner)

dropDatabase :: DatabaseName -> DB ()
dropDatabase name = do
  connection <- view postgreSQLConnectionL
  void $ liftIO $ execute connection [sql|DROP DATABASE ?|] (Only name)

doesDatabaseExist :: DatabaseName -> DB Bool
doesDatabaseExist (DatabaseName name) = do
  connection <- view postgreSQLConnectionL
  (length @[] @[Text] >>> (> 0))
    <$> liftIO
      ( query
          connection
          [sql|SELECT datname FROM pg_catalog.pg_database WHERE datname = ?|]
          (Only name)
      )

doesTableExist :: DatabaseTable -> DB Bool
doesTableExist table = do
  connection <- view postgreSQLConnectionL
  (length @[] @[Text] >>> (> 0))
    <$> liftIO
      ( query
          connection
          [sql|SELECT tablename FROM pg_catalog.pg_tables WHERE tablename = ?|]
          (Only $ unDatabaseTable table)
      )
