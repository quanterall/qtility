module Database.PostgreSQL.Simple.Migration
  ( createMigrationDatabase,
  )
where

import Database.PostgreSQL.Simple ()
import Database.PostgreSQL.Simple.Migration.Queries
import Database.PostgreSQL.Simple.Migration.Types
import Database.PostgreSQL.Simple.Utilities
import Database.PostgreSQL.Simple.Utilities.Types
import Qtility
import RIO.Directory (listDirectory)
import RIO.FilePath (takeBaseName, takeExtension, (</>))
import qualified RIO.List.Partial as PartialList
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import RIO.Time (UTCTime)
import qualified RIO.Time as Time

createMigrationDatabase ::
  (MonadIO m, MonadThrow m, MonadReader env m, HasPostgresqlMasterPool env) =>
  DatabaseOwner ->
  FilePath ->
  m [Migration]
createMigrationDatabase owner migrationsPath = do
  migrationFilenames <-
    filter (takeExtension >>> (== ".sql")) <$> liftIO (listDirectory migrationsPath)
  createDatabaseIfNotExists "migrations" owner
  migrations <- forM migrationFilenames $ \filename -> do
    (timestamp, name) <- parseMigrationName filename
    migrationText <- liftIO $ readFileUtf8 $ migrationsPath </> filename
    (up, down) <- parseMigrationText filename migrationText

    pure $
      Migration
        { _migrationName = Text.pack name,
          _migrationFilename = filename,
          _migrationUpStatement = up,
          _migrationDownStatement = down,
          _migrationIsApplied = False,
          _migrationTimestamp = timestamp
        }
  runMasterDB $ insertMigrations migrations
  pure migrations

parseMigrationName :: (MonadThrow m) => FilePath -> m (UTCTime, FilePath)
parseMigrationName filename = do
  let name = takeBaseName filename
      filenameSplit = name & Text.pack & PartialText.splitOn "_-_" & fmap Text.unpack
  unless (length filenameSplit == 2) $ throwM $ MigrationIncorrectFilename filename
  migrationTime <-
    filenameSplit
      & PartialList.head
      & Time.parseTimeM True Time.defaultTimeLocale timeFormat
      & fromPureMaybeM (MigrationIncorrectFilename filename)
  pure (migrationTime, name)

parseMigrationText :: (MonadThrow m) => FilePath -> Text -> m (Text, Text)
parseMigrationText filename text = do
  let migrationComponents = PartialText.splitOn "---- DOWN ----" text
  unless (length migrationComponents == 2) $ throwM $ MigrationIncorrectFormat filename
  let migrationUp = PartialList.head migrationComponents
      migrationDown = PartialList.last migrationComponents
  pure (migrationUp, migrationDown)

timeFormat :: String
timeFormat = "%Y-%m-%d_%H-%M-%S"
