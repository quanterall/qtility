{-# LANGUAGE TemplateHaskell #-}

module MigrationSpec.Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Postgres.Temp as TemporaryPostgres
import Qtility
import Qtility.Database (HasPostgresqlMasterPool (..), HasPostgresqlPool (..))
import Qtility.Database.Types (DatabaseName)
import Qtility.FileSystem (ReadFileSystem (..))
import RIO.Directory (doesDirectoryExist, doesFileExist, listDirectory)

runTestMonad :: TestState -> TestMonad a -> IO a
runTestMonad state action = action & unTestMonad & runRIO state

data TestState = TestState
  { _testStatePool :: !(Pool Connection),
    _testStateMasterPool :: !(Pool Connection),
    _testStateDatabaseName :: !DatabaseName,
    _testStateDatabase :: TemporaryPostgres.DB
  }
  deriving (Generic)

newtype TestMonad a = TestMonad {unTestMonad :: RIO TestState a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestState, MonadThrow)

foldMapM makeLenses [''TestState]

instance HasPostgresqlPool TestState where
  postgresqlPoolL = testStatePool

instance HasPostgresqlMasterPool TestState where
  postgresqlMasterPoolL = testStateMasterPool

instance ReadFileSystem TestMonad where
  listDirectoryM = listDirectory
  readFileM = readFileUtf8
  readByteStringFileM = readFileM >>> fmap encodeUtf8
  doesDirectoryExistM = doesDirectoryExist
  doesFileExistM = doesFileExist
