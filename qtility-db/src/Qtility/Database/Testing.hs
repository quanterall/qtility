module Qtility.Database.Testing
  ( createTemporaryDatabaseConnectionPool,
  )
where

import Data.Monoid (getLast)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import Database.PostgreSQL.Simple.Options (host, port)
import Database.Postgres.Temp (DB)
import qualified Database.Postgres.Temp as TemporaryPostgres
import Qtility
import Qtility.Database (createConnectionPool)
import Qtility.Database.Testing.Types
import Qtility.Database.Types (DatabaseConnections)

createTemporaryDatabaseConnectionPool ::
  (MonadIO m, MonadThrow m) =>
  DatabaseConnections ->
  m (DB, Pool Connection)
createTemporaryDatabaseConnectionPool connections = do
  temporaryDatabase <-
    TemporaryPostgres.defaultConfig
      & TemporaryPostgres.startConfig
      & fromEitherM
      & liftIO
  connectHost <-
    temporaryDatabase
      & TemporaryPostgres.toConnectionOptions
      & host
      & getLast
      & fromPureMaybeM (UnableToStartTemporaryPostgres "host not available in connection options")
  connectPort <-
    temporaryDatabase
      & TemporaryPostgres.toConnectionOptions
      & port
      & getLast
      & fromPureMaybeM (UnableToStartTemporaryPostgres "port not available in connection options")
      & fmap fromIntegral
  ConnectInfo
    { connectHost,
      connectPort,
      connectDatabase = "postgres",
      connectUser = "",
      connectPassword = "postgres"
    }
    & createConnectionPool connections
    & liftIO
    & fmap (temporaryDatabase,)
