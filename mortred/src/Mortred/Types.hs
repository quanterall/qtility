module Mortred.Types where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import RIO
import System.Process.Typed (Process)
import Test.WebDriver (WD)

data SessionDependencyConfiguration = SessionDependencyConfiguration
  { chromeBinary :: FilePath,
    chromeDriverBinary :: FilePath,
    seleniumPath :: SeleniumPath
  }
  deriving (Eq, Show)

data SeleniumProcess = SeleniumProcess
  { xvfbProcess :: XvfbProcess,
    process :: Process () () (),
    port :: SeleniumPort
  }
  deriving (Show)

data XvfbProcess = XvfbProcess
  { displayNumber :: DisplayNumber,
    process :: Process () () ()
  }
  deriving (Show)

newtype DisplayNumber = DisplayNumber {unDisplayNumber :: Int}
  deriving (Eq, Show)

-- | Represents the full path to a Selenium JAR-file.
newtype SeleniumPath = SeleniumPath {unSeleniumPath :: FilePath}
  deriving (Eq, Show)

newtype SeleniumPort = SeleniumPort {unSeleniumPort :: Int}
  deriving (Eq, Show)

newtype PortNumber = PortNumber {unPortNumber :: Int}
  deriving (Eq, Show)

newtype Url = Url {unUrl :: String}
  deriving (Eq, Show)

newtype MajorVersion = MajorVersion {unMajorVersion :: Int}
  deriving (Eq, Show)

newtype Milliseconds = Milliseconds {unMilliseconds :: Int}
  deriving (Eq, Show)

newtype ChromeVersion = ChromeVersion {unChromeVersion :: MajorVersion}
  deriving (Eq, Show)

newtype ChromeDriverVersion = ChromeDriverVersion {unChromeDriverVersion :: MajorVersion}
  deriving (Eq, Show)

-- | Represents a scraping queue with a queue to interact with, a list of associated Selenium
-- processes and a list of associated worker threads ('Async').
data ScrapingQueue a = ScrapingQueue
  { queue :: TBMQueue (ScrapingRequest a),
    processes :: [SeleniumProcess],
    threads :: [Async ()]
  }

-- | Represents a scraping request with an action to perform and a slot in which to put the result.
-- This is sent to a 'ScrapingQueue'.
data ScrapingRequest a = ScrapingRequest
  { action :: WD a,
    resultSlot :: MVar (Either SomeException a)
  }
