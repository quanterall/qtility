{-# LANGUAGE QuasiQuotes #-}

module Database.PostgreSQL.Simple.Migration.Queries where

import Database.PostgreSQL.Simple (Connection, executeMany)
import Database.PostgreSQL.Simple.Migration.Types
import Database.PostgreSQL.Simple.SqlQQ (sql)
import RIO

insertMigrations :: [Migration] -> Connection -> IO Int64
insertMigrations migrations conn =
  executeMany
    conn
    [sql|
      INSERT INTO migrations (name, up_statement, down_statement, filename, timestamp, is_applied)
      VALUES (?, ?, ?, ?, ?, ?)
     |]
    migrations
