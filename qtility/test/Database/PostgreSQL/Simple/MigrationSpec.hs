module Database.PostgreSQL.Simple.MigrationSpec where

import Data.Pool (destroyAllResources)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.Migration.Queries
import Database.PostgreSQL.Simple.MigrationSpec.Types
import Database.PostgreSQL.Simple.Utilities
import Database.PostgreSQL.Simple.Utilities.Queries (createDatabaseIfNotExists)
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
    runMasterDB' $ do
      createDatabaseIfNotExists (DatabaseName "qtility-test") (DatabaseOwner "postgres")
    runDB $ do
      createMigrationTableIfNotExists Nothing
      removeAllMigrations Nothing
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
  pure TestState {_testStatePool = pool, _testStateMasterPool = masterPool}

destroyPools :: TestState -> IO ()
destroyPools state = do
  state ^. testStateMasterPool & destroyAllResources
  state ^. testStatePool & destroyAllResources

withScaffolding :: SpecWith TestState -> Spec
withScaffolding = afterAll destroyPools >>> beforeAll createTestState

spec :: Spec
spec = do
  withScaffolding $
    describe "`createMigrationTable`" $ do
      it "creates a migration table that contains all files in given folder" $ \state -> do
        migrations <-
          runTestMonad state $
            createMigrationTable Nothing "test/test-data/migrations"
        length migrations `shouldBe` 1
        runTestMonad state (runDB $ getMigrations Nothing) `shouldReturn` migrations
