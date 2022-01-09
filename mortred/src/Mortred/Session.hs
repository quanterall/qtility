module Mortred.Session
  ( startSession,
    startSessions,
    SessionMode (..),
    SessionStartResult (..),
    SessionStartError (..),
    stopSession,
    waitRunSession,
    SessionRunTimedOut (..),
    webdriverConfig,
    createSessionPool,
    checkInSession,
    checkOutSession,
    withSession,
    waitForScrapingRequest,
  )
where

import Mortred.Selenium
import Mortred.Types
import Mortred.Types.Errors
import Mortred.Xvfb
import Network.HTTP.Client (HttpException)
import Qtility
import System.Process.Typed (stopProcess)
import Test.WebDriver (Browser (..), WD, WDConfig (..), defaultConfig, runSession, useBrowser)

-- | Creates a pool of sessions that can be checked out so that one can run scraping actions. Throws
-- 'SessionStartError' on failure and the already started sessions are cleaned up.
createSessionPool ::
  (MonadUnliftIO m, MonadThrow m, PrimMonad m, MonadFail m) =>
  PortNumber ->
  Int ->
  SeleniumPath ->
  m (TBMQueue SeleniumProcess)
createSessionPool startPortNumber count seleniumPath = do
  sessions <- startSessions startPortNumber count seleniumPath
  queue <- liftIO $ newTBMQueueIO count
  forM_ sessions $ \session ->
    liftIO $ atomically $ writeTBMQueue queue session
  pure queue

-- | Checks out a session from the pool, throwing 'SessionPoolClosed' if the pool is closed. If the
-- queue currently has no sessions, it will block until there are new ones.
checkOutSession :: (MonadUnliftIO m, MonadThrow m) => TBMQueue SeleniumProcess -> m SeleniumProcess
checkOutSession sessions = do
  fromMaybeM SessionPoolClosed $ liftIO $ atomically $ readTBMQueue sessions

-- | Checks a session into the pool. If this were to mean that the queue exceeds the bounds
-- (somehow), this blocks.
checkInSession :: (MonadUnliftIO m) => TBMQueue SeleniumProcess -> SeleniumProcess -> m ()
checkInSession sessions session = do
  liftIO $ atomically $ writeTBMQueue sessions session

-- | Executes an action with a session. If an exception is thrown along the way, the session is
-- automatically checked in again.
withSession ::
  (MonadUnliftIO m, MonadThrow m) =>
  TBMQueue SeleniumProcess ->
  (SeleniumProcess -> m a) ->
  m a
withSession sessions = do
  bracket (checkOutSession sessions) (checkInSession sessions)

-- | Takes a 'SeleniumProcess' and runs a 'WD' action using it, returning the result.
waitForScrapingRequest :: (MonadUnliftIO m, MonadThrow m) => SeleniumProcess -> WD a -> m a
waitForScrapingRequest seleniumProcess action = do
  waitRunSession (Milliseconds 5000) (webdriverConfig $ seleniumProcess ^. spPort) action

-- | Tries to start a session, throwing a 'SessionStartError' if it fails. This includes exceptions
-- for starting Xvfb, Selenium and reading process output to determine if we have compatible
-- webdriver versions, etc.
startSession ::
  (MonadThrow m, MonadUnliftIO m, PrimMonad m, MonadFail m) =>
  SessionMode ->
  SeleniumPath ->
  m SessionStartResult
startSession SessionOnDemand seleniumPath = do
  xvfbProcess <- mapExceptionM XvfbSessionError startXvfb
  (StartedOnDemand <$> startSelenium xvfbProcess seleniumPath)
    `catch` ( \(e :: SeleniumStartError) -> do
                xvfbProcess ^. xpProcess & stopProcess
                throwM $ SeleniumSessionError e
            )
startSession (SessionAlreadyStarted seleniumPort) _seleniumPath = do
  pure $ PremadeSession seleniumPort

stopSession :: (MonadUnliftIO m) => SeleniumProcess -> m ()
stopSession seleniumProcess = do
  seleniumProcess ^. spProcess & stopProcess
  seleniumProcess ^. spXvfbProcess . xpProcess & stopProcess

-- | @startSessions 0 6 '$' 'SeleniumPath' "./selenium.jar"@ attempts to start 6 sessions with
-- associated Xvfb & Selenium processes, starting at @0@ for the display number (up to 6) and @4444@
-- for the selenium port, up to @4444 + 6@. If any of the starts fail, the already started sessions
-- are stopped and a 'SessionStartError' is thrown.
startSessions ::
  (MonadUnliftIO m, MonadThrow m, PrimMonad m, MonadFail m) =>
  PortNumber ->
  Int ->
  SeleniumPath ->
  m [SeleniumProcess]
startSessions (PortNumber startPortNumber) count seleniumPath = do
  startedSessions <- newIORef []
  startSessions' startedSessions `onException` do
    alreadyStartedSessions <- readIORef startedSessions
    forM_ alreadyStartedSessions stopSession
  where
    startSessions' sessions =
      forM [startPortNumber .. startPortNumber + count - 1] $ \x -> do
        xvfbProcess <- mapExceptionM XvfbSessionError $ startXvfbWithDisplay $ DisplayNumber x
        session <-
          startSeleniumOnPort (SeleniumPort $ PortNumber $ 4444 + x) xvfbProcess seleniumPath
            `onException` liftIO (xvfbProcess ^. xpProcess & stopProcess)
        modifyIORef' sessions (session :)
        pure session

-- | Wait N 'Milliseconds' for an action to succeed with a given WebDriver configuration. Throws a
-- 'SessionRunTimedout' exception if the action does not succeed within the alloted time.
waitRunSession ::
  forall m a.
  (MonadThrow m, MonadUnliftIO m) =>
  Milliseconds ->
  WDConfig ->
  WD a ->
  m a
waitRunSession milliseconds@(Milliseconds ms) configuration@WDConfig {wdPort, wdHost} action = do
  fromMaybeM (SessionRunTimedOut wdPort wdHost milliseconds) timeoutRun
  where
    timeoutRun = timeout (ms * 1000) runSession'
    runSession' = do
      result <- try $ liftIO $ runSession configuration action
      either handleHttpException pure result
    handleHttpException :: HttpException -> m a
    handleHttpException _e = runSession'

webdriverConfig :: SeleniumPort -> WDConfig
webdriverConfig (SeleniumPort port) =
  defaultConfig {wdPort = _unPortNumber port} & useBrowser chromeNoSandbox

chromeNoSandbox :: Browser
chromeNoSandbox =
  Chrome
    { chromeDriverVersion = Nothing,
      -- @NOTE: The `--disable-dev-shm-usage` was needed because we were getting crashes when taking
      -- screenshots with a maximized window. I've yet to understand why this is the case.
      chromeOptions = ["--no-sandbox", "--disable-dev-shm-usage"],
      chromeBinary = Nothing,
      chromeExtensions = [],
      chromeExperimentalOptions = mempty
    }
