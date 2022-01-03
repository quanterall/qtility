{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Mortred.Types where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Lens.TH (makeLenses)
import RIO
import System.Process.Typed (Process)
import Test.WebDriver (WD)

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

newtype DisplayNumber = DisplayNumber {_unDisplayNumber :: Int}
  deriving (Eq, Show)

-- | Represents the full path to a Selenium JAR-file.
newtype SeleniumPath = SeleniumPath {_unSeleniumPath :: FilePath}
  deriving (Eq, Show)

newtype SeleniumPort = SeleniumPort {_unSeleniumPort :: Int}
  deriving (Eq, Show)

newtype PortNumber = PortNumber {_unPortNumber :: Int}
  deriving (Eq, Show)

newtype Url = Url {_unUrl :: String}
  deriving (Eq, Show)

newtype MajorVersion = MajorVersion {_unMajorVersion :: Int}
  deriving (Eq, Show)

newtype Milliseconds = Milliseconds {_unMilliseconds :: Int}
  deriving (Eq, Show)

newtype ChromeVersion = ChromeVersion {_unChromeVersion :: MajorVersion}
  deriving (Eq, Show)

newtype ChromeDriverVersion = ChromeDriverVersion {_unChromeDriverVersion :: MajorVersion}
  deriving (Eq, Show)

-- | Represents a scraping queue with a queue to interact with, a list of associated Selenium
-- processes and a list of associated worker threads ('Async').
data ScrapingQueue a = ScrapingQueue
  { _sqQueue :: TBMQueue (ScrapingRequest a),
    _sqProcesses :: [SeleniumProcess],
    _sqThreads :: [Async ()]
  }

-- | Represents a scraping request with an action to perform and a slot in which to put the result.
-- This is sent to a 'ScrapingQueue'.
data ScrapingRequest a = ScrapingRequest
  { _srAction :: WD a,
    _srResultSlot :: MVar (Either SomeException a)
  }

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
    ''ChromeDriverVersion,
    ''ScrapingQueue,
    ''ScrapingRequest
  ]
