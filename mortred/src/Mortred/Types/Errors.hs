module Mortred.Types.Errors where

import Data.Conduit.Serialization.Binary (ParseError)
import Mortred.Types
import Network.HTTP.Client (HttpException (..))
import Qtility

data XvfbStartError
  = XvfbProcessError IOException
  | UnableToAllocateDisplay
  deriving (Eq, Show)

instance Exception XvfbStartError

-- | Defines how to handle potential starting up of Xvfb & Selenium.
data SessionMode
  = -- | Start Xvfb and Selenium.
    SessionOnDemand
  | -- | Don't start Xvfb and Selenium.
    SessionAlreadyStarted SeleniumPort
  deriving (Eq, Show)

-- | The result of successfully starting a session.
data SessionStartResult
  = -- | Xvfb and Selenium were started and here is the Selenium process.
    StartedOnDemand !SeleniumProcess
  | -- | The session was premade and here is the port to connect to.
    PremadeSession !SeleniumPort
  deriving (Show)

data SessionStartError
  = XvfbSessionError XvfbStartError
  | SeleniumSessionError SeleniumStartError
  deriving (Show)

-- | Error representing a failure to set up @chromedriver@.
data ChromeDriverSetupError
  = -- | Unable to find @google-chrome@, which means we don't know which @chromedriver@ version to
    -- download.
    ChromeBinaryNotFound
  | -- | The chrome version we have has no valid @chromedriver@ version to download.
    NoValidChromeDriverUrl ChromeVersion
  | -- | The extracted major version of @google-chrome@ or @chromedriver@ is not supported.
    UnsupportedMajorVersion Text
  | -- | Unable to read the output of @google-chrome --version@.
    BadChromeVersionOutput LByteString
  | -- | Something went wrong when getting the output of @google-chrome --version@.
    UnableToReadChromeProcess FilePath IOException
  | -- | Something went wrong when downloading @chromedriver@.
    DownloadError Url HttpException
  | -- | Something went wrong when unzipping @chromedriver@.
    UnzipError ParseError
  deriving (Show)

instance Exception ChromeDriverSetupError

-- | Represents a failure to start Selenium.
data SeleniumStartError
  = -- | Unable to find Selenium at the given 'SeleniumPath'.
    SeleniumNotFound SeleniumPath
  | -- | We were unable to fulfill the pre-requisites of setting up @chromedriver@.
    SeleniumStartChromeDriverError ChromeDriverSetupError
  | -- | The @google-chrome@ and @chromedriver@ we have do not have matching major versions.
    VersionMismatch ChromeVersion ChromeDriverVersion
  deriving (Show)

instance Exception SeleniumStartError

instance Exception SessionStartError

data SessionRunTimedOut = SessionRunTimedOut
  { _srtoPort :: Int,
    _srtoHost :: String,
    _srtoMilliseconds :: Milliseconds
  }
  deriving (Show)

instance Exception SessionRunTimedOut

data SessionPoolClosed = SessionPoolClosed
  deriving (Show)

instance Exception SessionPoolClosed
