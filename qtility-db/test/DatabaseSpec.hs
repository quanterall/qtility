{-# LANGUAGE QuasiQuotes #-}

module DatabaseSpec where

import Data.Pool (destroyAllResources)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (ConnectInfo (..), Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import DatabaseSpec.Types
import Qtility
import Qtility.Database
import Qtility.Database.Migration
import Qtility.Database.Migration.Queries
import Qtility.Database.Queries
import Qtility.Database.Types
import qualified RIO.Text as Text
import Test.Hspec

createTestState :: IO TestState
createTestState = do
  randomDBSuffix <- nextRandom
  masterPool <-
    createConnectionPool
      (DatabaseConnections 1)
      ( ConnectInfo
          { connectHost = "localhost",
            connectPort = 5432,
            connectUser = "postgres",
            connectPassword = "postgres",
            connectDatabase = "postgres"
          }
      )
  let dbName = DatabaseName $ "qtility-database-spec-" <> (randomDBSuffix & show & fromString)
  runRIO masterPool $ runMasterDB' $ createDatabase dbName (DatabaseOwner "postgres")
  pool <-
    createConnectionPool
      (DatabaseConnections 1)
      ( ConnectInfo
          { connectHost = "localhost",
            connectPort = 5432,
            connectUser = "postgres",
            connectPassword = "postgres",
            connectDatabase = dbName & unDatabaseName & decodeUtf8Lenient & Text.unpack
          }
      )
  migrations <- runRIO pool $ createMigrationTable Nothing "test/DatabaseSpec/migrations"
  runRIO pool $ runDB $ applyMigrations Nothing migrations
  pure
    TestState
      { _testStatePool = pool,
        _testStateMasterPool = masterPool,
        _testStateDatabaseName = dbName
      }

destroyState :: TestState -> IO ()
destroyState state = do
  state ^. testStatePool & destroyAllResources
  runRIO state $ do
    runMasterDB' $ dropDatabase (state ^. testStateDatabaseName)
  state ^. testStateMasterPool & destroyAllResources

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
          `shouldThrow` (== DBNoResults query' (Only ("test2" :: Text)))
      it "Throws when we have too many results" $ \state -> do
        let query' = [sql| SELECT name FROM test_has_many WHERE name ILIKE ?|]
        runTestMonad
          state
          (runDB (queryOne @(Only Text) @(Only Text) query' (Only ("test%" :: Text))))
          `shouldThrow` (== DBTooManyResults query' (Only ("test%" :: Text)))

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
          `shouldThrow` (== DBNoResults query' (Only ("test2" :: Text)))

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
