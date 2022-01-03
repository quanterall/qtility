module Mortred.ScrapingQueue
  ( createScrapingQueue,
    destroyScrapingQueue,
    scheduleScrapingRequest,
    scheduleAndWaitForScrapingRequest,
  )
where

import Control.Concurrent.STM.TBMQueue (newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Mortred.Session
import Mortred.Types
import RIO
import Test.WebDriver (WD)

-- | @createScrapingQueue count '$' 'SeleniumPath' "./selenium.jar"@ creates a worker pool with
-- @count@ workers, that can be interacted with via a 'ScrapingQueue'.
createScrapingQueue ::
  (MonadUnliftIO m, MonadThrow m, PrimMonad m, MonadFail m) =>
  PortNumber ->
  Int ->
  SeleniumPath ->
  m (ScrapingQueue a)
createScrapingQueue startPortNumber count seleniumPath = do
  processes <- startSessions startPortNumber count seleniumPath
  queue <- liftIO $ newTBMQueueIO $ count * 2
  -- @TODO: maybe use `link` to make sure the calling thread is notified when one of the worker
  -- threads go down?
  threads <- liftIO $ forM processes $ workerLoop queue >>> async
  pure $ ScrapingQueue {_sqQueue = queue, _sqProcesses = processes, _sqThreads = threads}
  where
    workerLoop queue process = do
      maybeScrapingRequest <- atomically $ readTBMQueue queue
      forM_ maybeScrapingRequest $ \scrapingRequest -> do
        result <- tryAny $ scrapingRequest ^. srAction & waitForScrapingRequest process
        scrapingRequest ^. srResultSlot & (`putMVar` result)
        workerLoop queue process

-- | Destroys a scraping queue, cleaning up all the sessions and workers associated with it.
destroyScrapingQueue :: (MonadUnliftIO m) => ScrapingQueue a -> m ()
destroyScrapingQueue scrapingQueue = do
  scrapingQueue ^. sqThreads & mapM_ cancel
  scrapingQueue ^. sqProcesses & mapM_ stopSession

-- | Schedules a scraping request to be executed on a queue, returning the 'MVar' that will have the
-- result put into it.
scheduleScrapingRequest ::
  (MonadUnliftIO m) =>
  ScrapingQueue a ->
  WD a ->
  m (MVar (Either SomeException a))
scheduleScrapingRequest scrapingQueue action = do
  resultSlot <- liftIO newEmptyMVar
  let request = ScrapingRequest {_srAction = action, _srResultSlot = resultSlot}
  atomically $ writeTBMQueue (scrapingQueue ^. sqQueue) request
  pure resultSlot

-- | Schedules a scraping request to be executed on a queue, but waits immediately for it to finish
-- and returns the result.
scheduleAndWaitForScrapingRequest ::
  (MonadUnliftIO m) =>
  ScrapingQueue a ->
  WD a ->
  m (Either SomeException a)
scheduleAndWaitForScrapingRequest queue = do
  scheduleScrapingRequest queue >=> takeMVar
