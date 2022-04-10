{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.MigrationSpec.Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Utilities (HasPostgresqlMasterPool (..), HasPostgresqlPool (..))
import Qtility

runTestMonad :: TestState -> TestMonad a -> IO a
runTestMonad state action = action & unTestMonad & runRIO state

data TestState = TestState
  { _testStatePool :: !(Pool Connection),
    _testStateMasterPool :: !(Pool Connection)
  }

newtype TestMonad a = TestMonad {unTestMonad :: RIO TestState a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestState, MonadThrow)

foldMapM makeLenses [''TestState]

instance HasPostgresqlPool TestState where
  postgresqlPoolL = testStatePool

instance HasPostgresqlMasterPool TestState where
  postgresqlMasterPoolL = testStateMasterPool
