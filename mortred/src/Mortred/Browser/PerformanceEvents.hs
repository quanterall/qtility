-- | Handles the retrieval and decoding of performance events. These generally have to do with
-- how fast the page has loaded, how fast different resources have loaded and landmark events like
-- the page being fully loaded.
module Mortred.Browser.PerformanceEvents where

import Data.Aeson (FromJSON (..), defaultOptions, genericParseJSON, withObject, (.:))
import RIO
import Test.WebDriver (WD)
import qualified Test.WebDriver as WebDriver

data PerformanceNavigationTimingPayload = PerformanceNavigationTimingPayload
  { domComplete :: !Float,
    domInteractive :: !Float,
    loadEventEnd :: !Float,
    loadEventStart :: !Float,
    redirectCount :: !Int,
    redirectStart :: !Float,
    redirectEnd :: !Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON PerformanceNavigationTimingPayload where
  parseJSON = genericParseJSON defaultOptions

data PerformanceResourceTimingPayload = PerformanceResourceTimingPayload
  { initiatorType :: !Text,
    name :: !Text,
    entryType :: !Text,
    startTime :: !Float,
    duration :: !Float,
    redirectStart :: !Float,
    redirectEnd :: !Float,
    fetchStart :: !Float,
    domainLookupStart :: !Float,
    domainLookupEnd :: !Float,
    connectStart :: !Float,
    connectEnd :: !Float,
    secureConnectionStart :: !Float,
    requestStart :: !Float,
    responseStart :: !Float,
    responseEnd :: !Float,
    transferSize :: !Int,
    encodedBodySize :: !Int,
    decodedBodySize :: !Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON PerformanceResourceTimingPayload where
  parseJSON = genericParseJSON defaultOptions

data PerformancePaintTimingPayload = PerformancePaintTimingPayload
  { name :: !Text,
    startTime :: !Float,
    duration :: !Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON PerformancePaintTimingPayload where
  parseJSON = genericParseJSON defaultOptions

data PerformanceMarkPayload = PerformanceMarkPayload
  { duration :: !Float,
    name :: !Text,
    startTime :: !Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON PerformanceMarkPayload where
  parseJSON = genericParseJSON defaultOptions

data PerformanceEvent
  = PerformanceNavigationTiming PerformanceNavigationTimingPayload
  | PerformanceResourceTiming PerformanceResourceTimingPayload
  | PerformancePaintTiming PerformancePaintTimingPayload
  | PerformanceMark PerformanceMarkPayload
  deriving (Eq, Show)

instance FromJSON PerformanceEvent where
  parseJSON value =
    withObject
      "PerformanceEvent"
      ( \o -> do
          eventType :: Text <- o .: "entryType"
          case eventType of
            "navigation" -> PerformanceNavigationTiming <$> parseJSON value
            "resource" -> PerformanceResourceTiming <$> parseJSON value
            "paint" -> PerformancePaintTiming <$> parseJSON value
            "mark" -> PerformanceMark <$> parseJSON value
            e -> fail $ "Unknown 'eventType': " <> show e
      )
      value

getPerformanceEvents :: WD [PerformanceEvent]
getPerformanceEvents =
  WebDriver.executeJS [] "return window.performance.getEntries()"
