{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Qtility.Database.Testing.Types where

import Qtility

newtype UnableToStartTemporaryPostgres = UnableToStartTemporaryPostgres
  { unUnableToStartTemporaryPostgres :: Text
  }
  deriving (Eq, Show)

instance Exception UnableToStartTemporaryPostgres

foldMapM makeWrapped [''UnableToStartTemporaryPostgres]
