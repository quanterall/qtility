module Qtility.Database.Migration where

import Qtility
import Qtility.Database
import Qtility.Database.Migration.Queries
import Qtility.Database.Types
import Qtility.FileSystem (ReadFileSystem (..))
import RIO.FilePath (takeBaseName, takeExtension, (</>))
import qualified RIO.List as List
import qualified RIO.List.Partial as PartialList
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import RIO.Time (UTCTime)
import qualified RIO.Time as Time

createMigrationTable ::
  (MonadIO m, MonadThrow m, MonadReader env m, ReadFileSystem m, HasPostgresqlPool env) =>
  Maybe DatabaseSchema ->
  FilePath ->
  m [Migration]
createMigrationTable maybeSchema migrationsPath = do
  maybe (pure ()) (createSchemaIfNotExists >>> runDB) maybeSchema
  migrations <- migrationsInDirectory migrationsPath
  runDB $ do
    createMigrationTableIfNotExists maybeSchema
    insertMigrations maybeSchema migrations

  pure migrations

migrationsInDirectory :: (ReadFileSystem m, MonadThrow m) => FilePath -> m [Migration]
migrationsInDirectory path = do
  migrationFilenames <-
    (filter (takeExtension >>> (== ".sql")) >>> List.sort) <$> listDirectoryM path
  forM migrationFilenames $ \filename -> do
    timestamp <- parseMigrationTimestamp filename
    migrationText <- readFileM $ path </> filename
    (up, down) <- parseMigrationText filename migrationText

    pure $
      Migration
        { _migrationFilename = filename,
          _migrationUpStatement = up,
          _migrationDownStatement = down,
          _migrationIsApplied = False,
          _migrationTimestamp = timestamp
        }

parseMigrationTimestamp :: (MonadThrow m) => FilePath -> m UTCTime
parseMigrationTimestamp filename = do
  let name = takeBaseName filename
      filenameSplit = name & Text.pack & PartialText.splitOn "_-_" & fmap Text.unpack
  unless (length filenameSplit == 2) $ throwM $ MigrationIncorrectFilename filename
  filenameSplit
    & PartialList.head
    & Time.parseTimeM True Time.defaultTimeLocale timeFormat
    & fromPureMaybeM (MigrationIncorrectFilename filename)

parseMigrationText :: (MonadThrow m) => FilePath -> Text -> m (Text, Text)
parseMigrationText filename text = do
  let migrationComponents = PartialText.splitOn "-- DOWN" text
  unless (length migrationComponents == 2) $ throwM $ MigrationIncorrectFormat filename
  let migrationUp = migrationComponents & PartialList.head & Text.strip
      migrationDown = migrationComponents & PartialList.last & Text.strip
  pure (migrationUp, migrationDown)

timeFormat :: String
timeFormat = "%Y-%m-%d_%H-%M-%S"
