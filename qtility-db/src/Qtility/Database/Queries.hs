{-# LANGUAGE QuasiQuotes #-}

module Qtility.Database.Queries where

import Database.PostgreSQL.Simple (Only (..), Query, execute, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Qtility.Database
import Qtility.Database.Types
import RIO
import RIO.List.Partial as PartialList

-- | Executes a query and expects exactly one returned result.
queryOne :: (ToRow q, FromRow r) => Query -> q -> DB r
queryOne query' queryData = do
  connection <- view postgreSQLConnectionL
  results <- liftIO $ query connection query' queryData

  when (null results) $ throwM $ DBNoResults query'
  when (length results > 1) $ throwM $ DBTooManyResults query'

  pure $ PartialList.head results

queryOne_ :: (FromRow r) => Query -> DB r
queryOne_ query' = do
  connection <- view postgreSQLConnectionL
  results <- liftIO $ query_ connection query'

  when (null results) $ throwM $ DBNoResults query'
  when (length results > 1) $ throwM $ DBTooManyResults query'

  pure $ PartialList.head results

-- | Executes a query and expects at least one result, possibly more.
querySome :: (ToRow q, FromRow r) => Query -> q -> DB [r]
querySome query' queryData = do
  connection <- view postgreSQLConnectionL
  results <- liftIO $ query connection query' queryData

  when (null results) $ throwM $ DBNoResults query'

  pure results

-- | Executes a query and expects zero or more results.
queryMany :: (ToRow q, FromRow r) => Query -> q -> DB [r]
queryMany query' queryData = do
  connection <- view postgreSQLConnectionL
  liftIO $ query connection query' queryData

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
