{-# LANGUAGE QuasiQuotes #-}

module DatabaseSpec where

import Data.Monoid (getLast)
import Data.Pool (destroyAllResources)
import Database.PostgreSQL.Simple (ConnectInfo (..), Only (..))
import Database.PostgreSQL.Simple.Options (Options (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.Postgres.Temp as TemporaryPostgres
import DatabaseSpec.Types
import Qtility
import Qtility.Database
import Qtility.Database.Migration
import Qtility.Database.Migration.Queries
import Qtility.Database.Queries
import Qtility.Database.Types
import Test.Hspec

newtype UnableToStartTemporaryPostgres = UnableToStartTemporaryPostgres String
  deriving (Show, Eq)

instance Exception UnableToStartTemporaryPostgres

createTestState :: IO TestState
createTestState = do
  temporaryDatabase <- fromEitherM TemporaryPostgres.start
  let databaseOptions = TemporaryPostgres.toConnectionOptions temporaryDatabase
      connectUser = ""
      connectPassword = "postgres"
      connectDatabase = "postgres"
  connectHost <-
    databaseOptions & host & getLast & fromPureMaybeM (UnableToStartTemporaryPostgres "host")
  connectPort <-
    databaseOptions
      & port
      & getLast
      & fromPureMaybeM (UnableToStartTemporaryPostgres "port")
      & fmap fromIntegral
  pool <-
    createConnectionPool
      (DatabaseConnections 1)
      ( ConnectInfo
          { connectHost,
            connectPort,
            connectUser,
            connectPassword,
            connectDatabase
          }
      )
  masterPool <-
    createConnectionPool
      (DatabaseConnections 1)
      ( ConnectInfo
          { connectHost,
            connectPort,
            connectUser,
            connectPassword,
            connectDatabase
          }
      )
  let state =
        TestState
          { _testStatePool = pool,
            _testStateMasterPool = masterPool,
            _testStateDatabaseName = connectDatabase & fromString & DatabaseName,
            _testStateDatabase = temporaryDatabase
          }
  migrations <- runRIO state $ createMigrationTable Nothing "test/DatabaseSpec/migrations"
  runRIO pool $ runDB $ applyMigrations Nothing migrations
  pure state

destroyState :: TestState -> IO ()
destroyState state = do
  state ^. testStatePool & destroyAllResources
  state ^. testStateMasterPool & destroyAllResources
  TemporaryPostgres.stop $ state ^. testStateDatabase

withScaffolding :: SpecWith TestState -> Spec
withScaffolding = after destroyState >>> before createTestState

spec :: Spec
spec = do
  withScaffolding $ do
    describe "`queryOne`" $ do
      it "Works with one result" $ \state -> do
        runTestMonad
          state
          ( runDB
              ( queryOne
                  [sql| SELECT name FROM test_has_one WHERE name = ?|]
                  (Only ("test1" :: Text))
              )
          )
          `shouldReturn` Only ("test1" :: Text)
        runTestMonad
          state
          (runDB (queryOne_ [sql| SELECT name FROM test_has_one|]))
          `shouldReturn` Only ("test1" :: Text)
      it "Throws when we have no results" $ \state -> do
        let query' = [sql| SELECT name FROM test_has_one WHERE name = ?|]
        runTestMonad
          state
          (runDB (queryOne @(Only Text) @(Only Text) query' (Only ("test2" :: Text))))
          `shouldThrow` (== DBNoResults query')
      it "Throws when we have too many results" $ \state -> do
        let query' = [sql| SELECT name FROM test_has_many WHERE name ILIKE ?|]
        runTestMonad
          state
          (runDB (queryOne @(Only Text) @(Only Text) query' (Only ("test%" :: Text))))
          `shouldThrow` (== DBTooManyResults query')

    describe "`querySome`" $ do
      it "Works with one result" $ \state -> do
        runTestMonad
          state
          ( runDB
              ( querySome
                  [sql| SELECT name FROM test_has_one WHERE name = ?|]
                  (Only ("test1" :: Text))
              )
          )
          `shouldReturn` [Only ("test1" :: Text)]
      it "Works with multiple results" $ \state -> do
        runTestMonad
          state
          ( runDB
              ( querySome
                  [sql| SELECT name FROM test_has_many WHERE name ILIKE ?|]
                  (Only ("test%" :: Text))
              )
          )
          `shouldReturn` [Only ("test1" :: Text), Only ("test2" :: Text)]
      it "Throws when we have no results" $ \state -> do
        let query' = [sql| SELECT name FROM test_has_one WHERE name = ?|]
        runTestMonad
          state
          (runDB (querySome @(Only Text) @(Only Text) query' (Only ("test2" :: Text))))
          `shouldThrow` (== DBNoResults query')

    describe "`queryMany`" $ do
      it "Works with one result" $ \state -> do
        runTestMonad
          state
          ( runDB
              ( queryMany
                  [sql| SELECT name FROM test_has_one WHERE name = ?|]
                  (Only ("test1" :: Text))
              )
          )
          `shouldReturn` [Only ("test1" :: Text)]
      it "Works with multiple results" $ \state -> do
        runTestMonad
          state
          ( runDB
              ( queryMany
                  [sql| SELECT name FROM test_has_many WHERE name ILIKE ?|]
                  (Only ("test%" :: Text))
              )
          )
          `shouldReturn` [Only ("test1" :: Text), Only ("test2" :: Text)]
      it "Works with no results" $ \state -> do
        runTestMonad
          state
          ( runDB
              ( queryMany
                  [sql| SELECT name FROM test_has_one WHERE name ILIKE ?|]
                  (Only ("test2" :: Text))
              )
          )
          `shouldReturn` ([] :: [Only Text])
