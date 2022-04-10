module Database.PostgreSQL.Simple.MigrationSpec where

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
  runRIO masterPool $
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
  pure TestState {_testStatePool = pool, _testStateMasterPool = masterPool}

spec :: Spec
spec = do
  beforeAll createTestState $
    describe "`createMigrationTable`" $ do
      it "creates a migration table that contains all files in given folder" $ \state -> do
        migrations <-
          runTestMonad state $
            createMigrationTable Nothing "test/test-data/migrations"
        length migrations `shouldBe` 1
        runTestMonad state (runDB $ getMigrations Nothing) `shouldReturn` migrations
