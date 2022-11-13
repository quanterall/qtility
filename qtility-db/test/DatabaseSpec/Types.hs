{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module DatabaseSpec.Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Qtility
import Qtility.Database (HasPostgresqlMasterPool (..), HasPostgresqlPool (..))
import Qtility.Database.Types (DatabaseName)
import Qtility.FileSystem (ReadFileSystem (..))
import RIO.FilePath (splitFileName)
import qualified RIO.Map as Map

runTestMonad :: TestState -> TestMonad a -> IO a
runTestMonad state action = action & unTestMonad & runRIO state

data TestState = TestState
  { _testStatePool :: Pool Connection,
    _testStateMasterPool :: Pool Connection,
    _testStateDatabaseName :: DatabaseName,
    _testStateFiles :: IORef (Map FilePath (Map FilePath Text))
  }
  deriving (Generic)

newtype TestMonad a = TestMonad {unTestMonad :: RIO TestState a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestState, MonadThrow)

foldMapM makeLenses [''TestState]

instance HasPostgresqlPool TestState where
  postgresqlPoolL = testStatePool

instance HasPostgresqlMasterPool TestState where
  postgresqlMasterPoolL = testStateMasterPool

instance ReadFileSystem (RIO TestState) where
  listDirectoryM path = do
    files <- view testStateFiles >>= readIORef
    files & Map.lookup path & maybe (error $ "No files in: " <> path) Map.keys & pure
  readFileM path = do
    files <- view testStateFiles >>= readIORef
    let fileContents = do
          let (dir, file) = splitFileName path
          directoryContents <- Map.lookup dir files
          Map.lookup file directoryContents
    fileContents & maybe (error $ "No file: " <> path) pure
  readByteStringFileM = readFileM >>> fmap encodeUtf8
  doesDirectoryExistM path = do
    files <- view testStateFiles >>= readIORef
    files & Map.member path & pure
  doesFileExistM path = do
    files <- view testStateFiles >>= readIORef
    let (dir, file) = splitFileName path
        directoryContents = Map.lookup dir files
    directoryContents & maybe False (Map.member file) & pure
