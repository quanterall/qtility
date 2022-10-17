module MigrationSpec where

import Control.Lens.Combinators (_head, _last)
import Data.Pool (destroyAllResources)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier (..))
import MigrationSpec.Types
import Qtility
import Qtility.Database
import Qtility.Database.Migration
import Qtility.Database.Migration.Queries
import Qtility.Database.Queries (createDatabase, doesTableExist, dropDatabase)
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
  let dbName = DatabaseName $ "qtility-test-" <> (randomDBSuffix & show & fromString)
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
  runRIO pool $ do
    runDB $ do
      createMigrationTableIfNotExists Nothing
      removeAllMigrations Nothing
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
    describe "`createMigrationTable`" $ do
      it "creates a migration table that contains all files in given folder" $ \state -> do
        migrations <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        length migrations `shouldBe` 1
        runTestMonad state (runDB $ getMigrations Nothing) `shouldReturn` migrations
        forM_ migrations $ \migration -> do
          (migration ^. migrationFilename, migration ^. migrationIsApplied)
            `shouldBe` (migration ^. migrationFilename, False)

    describe "`applyMigrations`" $ do
      it "applies all the passed in migrations if possible, none on errors" $ \state -> do
        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        migrations <- runTestMonad state $ runDB $ getMigrations Nothing
        runTestMonad
          state
          ( runDB $ do
              applyMigrations Nothing migrations
              doesTableExist "that_thing"
          )
          `shouldReturn` True

      it "works when you run one initial file, then another" $ \state -> do
        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        runTestMonad state (runDB $ doesTableExist "that_thing") `shouldReturn` False
        migrations <- runTestMonad state $ runDB $ getMigrations Nothing
        runTestMonad
          state
          ( runDB $ do
              applyMigrations Nothing migrations
              e1 <- doesTableExist "that_thing"
              e2 <- doesTableExist "other_thing"
              pure (e1, e2)
          )
          `shouldReturn` (True, False)

        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations2"
        newMigrations <- runTestMonad state $ runDB $ getMigrations Nothing
        newMigrations `shouldNotBe` migrations
        length migrations `shouldBe` 1
        length newMigrations `shouldBe` 2
        newMigrations ^? _head . migrationIsApplied `shouldBe` Just True
        newMigrations ^? _last . migrationIsApplied `shouldBe` Just False

        runTestMonad state (runDB $ getAppliedMigrations Nothing)
          `shouldReturn` (migrations & _head . migrationIsApplied .~ True)
        runTestMonad state (runDB $ getUnappliedMigrations Nothing)
          `shouldReturn` newMigrations ^.. _last

        runTestMonad
          state
          ( runDB $ do
              applyMigrations Nothing newMigrations
              e1 <- doesTableExist "that_thing"
              e2 <- doesTableExist "other_thing"
              pure (e1, e2)
          )
          `shouldReturn` (True, True)

        runTestMonad
          state
          ( runDB $ do
              rollbackLastNMigrations Nothing 1
              e1 <- doesTableExist "that_thing"
              e2 <- doesTableExist "other_thing"
              pure (e1, e2)
          )
          `shouldReturn` (True, False)

    describe "`rollbackLastNMigrations`" $ do
      it "Throws an error when there are no applied migrations" $ \state -> do
        _ <- runTestMonad state $ runDB $ getMigrations Nothing
        runTestMonad state (runDB $ rollbackLastNMigrations Nothing 1)
          `shouldThrow` (== NoMigrationsFound (QualifiedIdentifier Nothing "migrations"))

    describe "`updateMigration`" $ do
      it "Does not update the application state of the migration" $ \state -> do
        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        (m : _) <- runTestMonad state $ runDB $ getMigrations Nothing
        runTestMonad
          state
          ( runDB $ do
              oldMigration <-
                updateMigration Nothing $
                  m
                    & migrationIsApplied .~ True
                    & migrationUpStatement .~ "SELECT 'Alan Turing';"
              currentMigrations <- getMigrations Nothing
              pure (oldMigration, currentMigrations)
          )
          `shouldReturn` ( m,
                           [ m & migrationIsApplied .~ False
                               & migrationUpStatement .~ "SELECT 'Alan Turing';"
                           ]
                         )

      it "Throws an error when the migration doesn't exist" $ \state -> do
        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        (m : _) <- runTestMonad state $ runDB $ getMigrations Nothing
        runTestMonad
          state
          ( runDB $ do
              updateMigration Nothing (m & migrationFilename .~ "not-a-migration-we-can-find")
          )
          `shouldThrow` (== MigrationNotFound "not-a-migration-we-can-find")

    describe "`removeMigration`" $ do
      it "Removes the given migration" $ \state -> do
        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        (m : _) <- runTestMonad state $ runDB $ getMigrations Nothing
        runTestMonad
          state
          ( runDB $ do
              removeMigration Nothing (m ^. migrationFilename)
              getMigrations Nothing
          )
          `shouldReturn` []

      it "Should give an error when trying to remove non-existent migration" $ \state -> do
        _ <- runTestMonad state $ createMigrationTable Nothing "test/test-data/migrations1"
        runTestMonad state (runDB $ removeMigration Nothing "not-a-migration-we-can-find")
          `shouldThrow` (== MigrationNotFound "not-a-migration-we-can-find")
