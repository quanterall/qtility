module Main where

import Mortred.Browser.Commands (waitForVisibleElement)
import Mortred.Session (createSessionPool, waitForScrapingRequest, withSession)
import Mortred.Types
import Qtility.Time.Types (Seconds (..))
import RIO hiding (link)
import System.IO (print)
import Test.WebDriver (Selector (..), WD, closeSession, maximize, openPage, screenshot)

main :: IO ()
main = do
  sessionQueue <-
    liftIO $
      createSessionPool (PortNumber 5555) 3 $
        SeleniumPath "./selenium-server-standalone-2.53.1.jar"
  result <- withSession sessionQueue $ \s -> waitForScrapingRequest s fingerprintJS
  print result

fingerprintJS :: WD ()
fingerprintJS = do
  openPage "https://fingerprintjs.com/"
  maximize
  waitForVisibleElement (Seconds 5.0) $ ByCSS "span[class^='FpjsWidget-module--value--']"
  screenshotData <- screenshot
  liftIO $ writeFileBinary "screenshot.png" $ toStrictBytes screenshotData
  closeSession
