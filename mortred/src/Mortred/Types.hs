{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Mortred.Types where

import Control.Lens.TH (makeLenses)
import Qtility.TH.Lens (deriveWrappeds)
import RIO
import System.Process.Typed (Process)

-- | This is the display number that Xvfb uses.
newtype DisplayNumber = DisplayNumber {_unDisplayNumber :: Int}
  deriving (Eq, Show, Generic)

-- | Represents the full path to a Selenium JAR-file.
newtype SeleniumPath = SeleniumPath {_unSeleniumPath :: FilePath}
  deriving (Eq, Show, Generic)

newtype SeleniumPort = SeleniumPort {_unSeleniumPort :: PortNumber}
  deriving (Eq, Show, Generic)

newtype PortNumber = PortNumber {_unPortNumber :: Int}
  deriving (Eq, Show, Generic)

newtype Url = Url {_unUrl :: String}
  deriving (Eq, Show, Generic)

newtype MajorVersion = MajorVersion {_unMajorVersion :: Int}
  deriving (Eq, Show, Generic)

newtype Milliseconds = Milliseconds {_unMilliseconds :: Int}
  deriving (Eq, Show, Generic)

newtype ChromeVersion = ChromeVersion {_unChromeVersion :: MajorVersion}
  deriving (Eq, Show, Generic)

newtype ChromeDriverVersion = ChromeDriverVersion {_unChromeDriverVersion :: MajorVersion}
  deriving (Eq, Show, Generic)

data SessionConfiguration = SessionConfiguration
  { _scChromeBinary :: FilePath,
    _scChromeDriverBinary :: FilePath,
    _scSeleniumPath :: SeleniumPath
  }
  deriving (Eq, Show)

data SeleniumProcess = SeleniumProcess
  { _spXvfbProcess :: XvfbProcess,
    _spProcess :: Process () () (),
    _spPort :: SeleniumPort
  }
  deriving (Show)

data XvfbProcess = XvfbProcess
  { _xpDisplayNumber :: DisplayNumber,
    _xpProcess :: Process () () ()
  }
  deriving (Show)

foldMapM
  makeLenses
  [ ''SessionConfiguration,
    ''XvfbProcess,
    ''SeleniumProcess,
    ''DisplayNumber,
    ''SeleniumPath,
    ''SeleniumPort,
    ''PortNumber,
    ''Url,
    ''MajorVersion,
    ''Milliseconds,
    ''ChromeVersion,
    ''ChromeDriverVersion
  ]

deriveWrappeds
  [ ''DisplayNumber,
    ''SeleniumPath,
    ''SeleniumPort,
    ''PortNumber,
    ''Url,
    ''MajorVersion,
    ''Milliseconds,
    ''ChromeVersion,
    ''ChromeDriverVersion
  ]
