{-# LANGUAGE TemplateHaskell #-}

module Qtility.File.Types where

import Control.Lens.TH (makeLenses)
import RIO

data JsonFileDecodingError = JsonFileDecodingError
  { _jsonFileDecodingErrorPath :: FilePath,
    _jsonFileDecodingErrorReason :: String
  }
  deriving (Show, Eq)

foldMapM makeLenses [''JsonFileDecodingError]
