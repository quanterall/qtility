module Mortred.Scalpel
  ( scrapeElement,
  )
where

import RIO
import Test.WebDriver (Element, attr)
import Test.WebDriver.Class (WebDriver)
import Text.HTML.Scalpel (ScraperT, scrapeStringLikeT)

-- | Runs a 'ScraperT' on a 'Element'. This automatically extracts the inner HTML of the element and
-- applies the 'ScraperT' to it. "Test.WebDriver" already has utilities for finding elements, but
-- running a 'ScraperT' can be useful when a structure is more readily scraped with one.
scrapeElement :: (WebDriver m) => ScraperT Text m a -> Element -> m (Maybe a)
scrapeElement scraper e = do
  maybeHtml <- e `attr` "innerHTML"
  maybe (pure Nothing) (`scrapeStringLikeT` scraper) maybeHtml
