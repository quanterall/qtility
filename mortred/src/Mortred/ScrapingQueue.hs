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
  pure $ ScrapingQueue {queue, processes, threads}
  where
    workerLoop queue process = do
      maybeScrapingRequest <- atomically $ readTBMQueue queue
      case maybeScrapingRequest of
        Just ScrapingRequest {action, resultSlot} -> do
          result <- tryAny $ waitForScrapingRequest process action
          putMVar resultSlot result
          workerLoop queue process
        Nothing -> pure ()

-- | Destroys a scraping queue, cleaning up all the sessions and workers associated with it.
destroyScrapingQueue :: (MonadUnliftIO m) => ScrapingQueue a -> m ()
destroyScrapingQueue ScrapingQueue {processes, threads} = do
  forM_ threads cancel
  forM_ processes stopSession

-- | Schedules a scraping request to be executed on a queue, returning the 'MVar' that will have the
-- result put into it.
scheduleScrapingRequest ::
  (MonadUnliftIO m) =>
  ScrapingQueue a ->
  WD a ->
  m (MVar (Either SomeException a))
scheduleScrapingRequest ScrapingQueue {queue} action = do
  resultSlot <- liftIO newEmptyMVar
  atomically $ writeTBMQueue queue $ ScrapingRequest {action, resultSlot}
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
