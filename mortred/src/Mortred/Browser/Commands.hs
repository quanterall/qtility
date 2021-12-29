-- | Holds commands to execute client-side.
module Mortred.Browser.Commands where

import RIO
import Test.WebDriver
import Test.WebDriver.Commands.Wait

-- | @'waitForVisibleElement' waitTime selector@ waits for the element with the given 'Selector' to
-- be visible. When the timeout is reached, this throws a 'Timeout' exception. Note that both the
-- finding of the element and the visibility of it are waited for, meaning that an erroneous
-- selector will force the wait until the timeout is hit. For only waiting for visibility, use
-- @'waitUntil' timeout $ 'isDisplayed' element >>= 'expect'@
waitForVisibleElement :: Double -> Selector -> WD ()
waitForVisibleElement waitTime selector = do
  waitUntil waitTime $ do
    element <- findElem selector
    displayed <- isDisplayed element
    expect displayed

-- | @'waitForCompleteReadyState' timeout@ waits for the browser to set `document.readyState` to
-- ""complete"". Note that this does not mean that the page is fully loaded; we may still be waiting
-- for API calls to complete.
waitForCompleteReadyState :: Double -> WD ()
waitForCompleteReadyState waitTime = do
  waitUntil waitTime $ do
    readyState :: Text <- executeJS [] "return document.readyState"
    expect $ readyState == "complete"
