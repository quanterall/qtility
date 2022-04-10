module Database.PostgreSQL.Simple.MigrationSpec where

import Data.Pool (destroyAllResources)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.Migration.Queries
import Database.PostgreSQL.Simple.Migration.Types
import Database.PostgreSQL.Simple.MigrationSpec.Types
import Database.PostgreSQL.Simple.Utilities
import Database.PostgreSQL.Simple.Utilities.Queries
  ( createDatabaseIfNotExists,
    doesTableExist,
    dropDatabase,
  )
import Database.PostgreSQL.Simple.Utilities.Types
import Qtility
import Test.Hspec

createTestState :: IO TestState
createTestState = do
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
  runRIO masterPool $ do
    runMasterDB' $ dropDatabase (DatabaseName "qtility-test")
    runMasterDB' $
      createDatabaseIfNotExists (DatabaseName "qtility-test") (DatabaseOwner "postgres")
  pool <-
    createConnectionPool
      (DatabaseConnections 1)
      ( ConnectInfo
          { connectHost = "localhost",
            connectPort = 5432,
            connectUser = "postgres",
            connectPassword = "postgres",
            connectDatabase = "qtility-test"
          }
      )
  runRIO pool $ do
    runDB $ do
      createMigrationTableIfNotExists Nothing
      removeAllMigrations Nothing
  pure TestState {_testStatePool = pool, _testStateMasterPool = masterPool}

destroyPools :: TestState -> IO ()
destroyPools state = do
  state ^. testStateMasterPool & destroyAllResources
  state ^. testStatePool & destroyAllResources

withScaffolding :: SpecWith TestState -> Spec
withScaffolding = afterAll destroyPools >>> beforeAll createTestState

spec :: Spec
spec = do
  withScaffolding $ do
    describe "`createMigrationTable`" $ do
      it "creates a migration table that contains all files in given folder" $ \state -> do
        migrations <-
          runTestMonad state $
            createMigrationTable Nothing "test/test-data/migrations"
        length migrations `shouldBe` 1
        runTestMonad state (runDB $ getMigrations Nothing) `shouldReturn` migrations
        forM_ migrations $ \migration -> do
          (migration ^. migrationFilename, migration ^. migrationIsApplied)
            `shouldBe` (migration ^. migrationFilename, False)

    describe "`applyMigrations`" $ do
      it "applies all the passed in migrations if possible, none on errors" $ \state -> do
        _ <-
          runTestMonad state $
            createMigrationTable Nothing "test/test-data/migrations"
        migrations <- runTestMonad state $ runDB $ getMigrations Nothing
        doesExampleTableExist <- runTestMonad state $
          runDB $ do
            applyMigrations Nothing migrations
            doesTableExist "that_thing"
        doesExampleTableExist `shouldBe` True
