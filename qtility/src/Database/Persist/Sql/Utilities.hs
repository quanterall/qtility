module Database.Persist.Sql.Utilities where

import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool)
import RIO

class HasSqlConnectionPool env where
  sqlConnectionPoolL :: Lens' env ConnectionPool

instance HasSqlConnectionPool ConnectionPool where
  sqlConnectionPoolL = id

runDB :: (MonadIO m, MonadReader env m, HasSqlConnectionPool env) => SqlPersistM a -> m a
runDB a = do
  pool <- view sqlConnectionPoolL
  liftIO $ runSqlPersistMPool a pool
