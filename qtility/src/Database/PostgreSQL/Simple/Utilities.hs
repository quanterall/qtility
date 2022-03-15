module Database.PostgreSQL.Simple.Utilities where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Qtility

class HasPostgresqlPool env where
  postgresqlPoolL :: Lens' env (Pool Connection)

instance HasPostgresqlPool (Pool Connection) where
  postgresqlPoolL = id

runDB ::
  (MonadReader env m, MonadUnliftIO m, HasPostgresqlPool env) =>
  (Connection -> IO a) ->
  m a
runDB action = do
  pool <- view postgresqlPoolL
  liftIO $ withResource pool action
