-- | Example code for scraping Indeed
module Mortred.Indeed where

import Mortred.Session
import Mortred.Types
import Qtility.Data (fromMaybeM)
import Qtility.Time.Types (Milliseconds (..))
import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import Test.WebDriver.Commands

newtype IndeedKeyword = IndeedKeyword {unIndeedKeyword :: String}
  deriving (Eq, Show)

newtype IndeedCity = IndeedCity {unIndeedCity :: String}
  deriving (Eq, Show)

data NoJobId = NoJobId
  deriving (Eq, Show)

instance Exception NoJobId

-- | Represents a company location in a job posting.
data IndeedJobLocation
  = -- | Represents a posting that allows for remote work. If the position/company is only remote,
    -- the 'Maybe Text' will be 'Nothing'. If the company also offers office work, it will be
    -- 'Just locationText'.
    RemoteLocation (Maybe Text)
  | -- | Represents a location that has no mention of remote work.
    OnLocation Text
  deriving (Eq, Show)

data IndeedJobSummary = IndeedJobSummary
  { title :: Text,
    company :: Text,
    location :: IndeedJobLocation,
    jobId :: Maybe Text
  }
  deriving (Eq, Show)

scrapeIndeed ::
  (MonadUnliftIO m, MonadThrow m, PrimMonad m, MonadFail m) =>
  IndeedKeyword ->
  IndeedCity ->
  m [IndeedJobSummary]
scrapeIndeed keyword _city = do
  startResult <-
    startSession SessionOnDemand $ SeleniumPath "./selenium-server-standalone-2.53.1.jar"
  case startResult of
    (StartedOnDemand seleniumProcess) -> do
      waitRunSession (Milliseconds 10000) (seleniumProcess ^. spPort & webdriverConfig) doScrape
        `finally` stopSession seleniumProcess
    (PremadeSession port) -> do
      waitRunSession (Milliseconds 10000) (webdriverConfig port) doScrape
  where
    doScrape = do
      openPage $ "https://www.indeed.com/q-" <> unIndeedKeyword keyword <> "-jobs.html"
      jobBoxes <- findElems (ByCSS "a[id^='job_']")
      mapM parseIndeedJobsSummary jobBoxes
    parseIndeedJobsSummary e = do
      title <- findElemFrom e (ByClass "jobTitle") >>= getText
      company <- findElemFrom e (ByCSS "span.companyName") >>= getText
      location <- locationFromText <$> (findElemFrom e (ByCSS "div.companyLocation") >>= getText)
      jobId <- (Text.split (== '_') >>> List.lastMaybe) <$> fromMaybeM NoJobId (e `attr` "id")
      pure IndeedJobSummary {title, company, jobId, location}

locationFromText :: Text -> IndeedJobLocation
locationFromText "Remote" = RemoteLocation Nothing
locationFromText text
  | "Remote" `Text.isInfixOf` text =
    text & Text.split (== '\8226') & List.headMaybe & RemoteLocation
  | otherwise = OnLocation text
