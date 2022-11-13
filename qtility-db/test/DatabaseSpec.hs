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
import qualified RIO.Map as Map
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
  let migration1 =
        Text.unlines
          [ "CREATE TABLE \"test_has_one\" (",
            "  \"id\" bigserial PRIMARY KEY,",
            "  \"name\" text NOT NULL,",
            "  \"created_at\" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,",
            "  \"updated_at\" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP",
            ");",
            "INSERT INTO \"test_has_one\" (\"name\") VALUES ('test1');",
            "",
            "CREATE TABLE \"test_has_none\" (",
            "  \"id\" bigserial PRIMARY KEY,",
            "  \"name\" text NOT NULL,",
            "  \"created_at\" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,",
            "  \"updated_at\" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP",
            ");",
            "",
            "CREATE TABLE \"test_has_many\" (",
            "  \"id\" bigserial PRIMARY KEY,",
            "  \"name\" text NOT NULL,",
            "  \"created_at\" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,",
            "  \"updated_at\" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP",
            ");",
            "",
            "INSERT INTO \"test_has_many\" (\"name\") VALUES ('test1'), ('test2');",
            "",
            "-- DOWN",
            "",
            "DROP TABLE \"test_has_one\";",
            "DROP TABLE \"test_has_none\";",
            "DROP TABLE \"test_has_many\";"
          ]
  files <-
    newIORef $
      Map.fromList
        [ ( "test/DatabaseSpec",
            Map.fromList [("2022-05-02_10-34-18_-_create_and_seed_database.sql", migration1)]
          )
        ]
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
  let state =
        TestState
          { _testStatePool = pool,
            _testStateMasterPool = masterPool,
            _testStateDatabaseName = dbName,
            _testStateFiles = files
          }
  migrations <- runRIO state $ createMigrationTable Nothing "test/DatabaseSpec/migrations"
  runRIO pool $ runDB $ applyMigrations Nothing migrations
  pure state

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
