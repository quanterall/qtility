module Main where

import Mortred.Browser.Commands (waitForVisibleElement)
import Mortred.Session
  ( SessionMode (..),
    SessionStartResult (..),
    stopSession,
    tryStartSession,
    waitRunSession,
    webdriverConfig,
  )
import Mortred.Types
import Qtility.Data.Types (Seconds (..))
import RIO hiding (link)
import System.IO (putStrLn)
import Test.WebDriver (Selector (..), WD, closeSession, maximize, openPage, screenshot)

main :: IO ()
main = do
  maybeSeleniumProcess <-
    tryStartSession SessionOnDemand $ SeleniumPath "./selenium-server-standalone-2.53.1.jar"

  case maybeSeleniumProcess of
    Right (StartedOnDemand seleniumProcess@SeleniumProcess {port}) -> do
      waitRunSession (Milliseconds 10000) (webdriverConfig port) fingerprintJS
        `finally` stopSession seleniumProcess
    Right (PremadeSession port) -> do
      waitRunSession (Milliseconds 10000) (webdriverConfig port) fingerprintJS
    Left e ->
      putStrLn $ "Unable to start session: " <> show e

fingerprintJS :: WD ()
fingerprintJS = do
  openPage "https://fingerprintjs.com/"
  maximize
  waitForVisibleElement (Seconds 5.0) $ ByCSS "span[class^='FpjsWidget-module--value--']"
  screenshotData <- screenshot
  liftIO $ writeFileBinary "screenshot.png" $ toStrictBytes screenshotData
  closeSession
