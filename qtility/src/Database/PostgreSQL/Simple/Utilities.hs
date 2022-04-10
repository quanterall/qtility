-- | Utilities for dealing with `postgresql-simple`. This includes support for running queries and
-- statements if one has a 'Pool Connection' available in the current environment.
module Database.PostgreSQL.Simple.Utilities where

import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    close,
    connect,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Utilities.Types
import Qtility
import qualified RIO.Text as Text

class HasPostgresqlPool env where
  postgresqlPoolL :: Lens' env (Pool Connection)

instance HasPostgresqlPool (Pool Connection) where
  postgresqlPoolL = id

class HasPostgresqlMasterPool env where
  postgresqlMasterPoolL :: Lens' env (Pool Connection)

instance HasPostgresqlMasterPool (Pool Connection) where
  postgresqlMasterPoolL = id

class HasPostgreSQLConnection env where
  postgreSQLConnectionL :: Lens' env Connection

instance HasPostgreSQLConnection Connection where
  postgreSQLConnectionL = id

-- | Action that can be run via 'runDB' or 'runMasterDB'. This has access to a 'Connection' and
-- 'IO' only and is purposefully not just any environment.
newtype DB a = DB {unDB :: RIO Connection a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection)

-- | Runs an action against the connection pool that has been chosen for the main pool of the
-- application. This is different to 'runMasterDB', which (idiomatically) would run against the
-- @postgres@ database.
runDB ::
  (MonadReader env m, MonadIO m, HasPostgresqlPool env) =>
  DB a ->
  m a
runDB action = do
  pool <- view postgresqlPoolL
  liftIO $ runInTransaction pool action

runDB' ::
  (MonadReader env m, MonadIO m, HasPostgresqlPool env) =>
  DB a ->
  m a
runDB' action = do
  pool <- view postgresqlPoolL
  liftIO $ withResource pool $ \connection -> runRIO connection $ unDB action

runInTransaction ::
  (MonadIO m) =>
  Pool Connection ->
  DB a ->
  m a
runInTransaction pool action = do
  liftIO $
    withResource pool $ \connection ->
      withTransaction connection $ runRIO connection $ unDB action

-- | Runs an action against the master database of PostgreSQL, i.e. the @postgres@ database.
runMasterDB ::
  (MonadIO m, MonadReader env m, HasPostgresqlMasterPool env) =>
  DB a ->
  m a
runMasterDB action = do
  pool <- view postgresqlMasterPoolL
  liftIO $ runInTransaction pool action

runMasterDB' ::
  (MonadIO m, MonadReader env m, HasPostgresqlMasterPool env) =>
  DB a ->
  m a
runMasterDB' action = do
  pool <- view postgresqlMasterPoolL
  liftIO $ withResource pool $ \connection -> runRIO connection $ unDB action

-- | Creates a @'Pool' 'Connection'@ of 'DatabaseConnections' size for the given 'ConnectInfo'.
createConnectionPool :: (MonadIO m) => DatabaseConnections -> ConnectInfo -> m (Pool Connection)
createConnectionPool connections connectInfo =
  liftIO $ createPool (connect connectInfo) close 1 10 (connections ^. unwrap)

-- | Creates a @'Pool' 'Connection'@ of 'DatabaseConnections' size for a given 'RDSSecret' value.
createRDSConnectionPool :: (MonadIO m) => DatabaseConnections -> RDSSecret -> m (Pool Connection)
createRDSConnectionPool connections secret = do
  let connectionInfo = rdsSecretToConnectInfo secret
  createConnectionPool connections connectionInfo

rdsSecretToConnectInfo :: RDSSecret -> ConnectInfo
rdsSecretToConnectInfo secret =
  ConnectInfo
    { connectHost = secret ^. rdsSecretHost & Text.unpack,
      connectPort = secret ^. rdsSecretPort & fromIntegral,
      connectUser = secret ^. rdsSecretUsername & Text.unpack,
      connectPassword = secret ^. rdsSecretPassword & Text.unpack,
      connectDatabase = secret ^. rdsSecretName & Text.unpack
    }
