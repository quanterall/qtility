module Mortred.Xvfb
  ( startXvfb,
    startXvfbWithDisplay,
    allocateDisplayNumber,
  )
where

import Mortred.Types
import Mortred.Types.Errors
import Qtility.Data (findM, fromMaybeM, unwrap)
import RIO
import RIO.Directory (doesFileExist)
import System.Process.Typed
  ( nullStream,
    proc,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
  )

-- | Attempts to start an `Xvfb` process. Throws 'XvfbStartError' on failure.
startXvfb :: (MonadThrow m, MonadUnliftIO m) => m XvfbProcess
startXvfb = do
  displayNumber <- fromMaybeM UnableToAllocateDisplay allocateDisplayNumber
  startXvfbWithDisplay displayNumber

startXvfbWithDisplay :: (MonadThrow m, MonadUnliftIO m) => DisplayNumber -> m XvfbProcess
startXvfbWithDisplay displayNumber = do
  let processConfiguration =
        proc "Xvfb" [":" <> (displayNumber ^. unwrap & show), "-screen", "0", "1920x1080x24"]
          & setStdin nullStream
          & setStdout nullStream
          & setStderr nullStream
  process <- mapExceptionM XvfbProcessError $ startProcess processConfiguration
  pure $ XvfbProcess {_xpDisplayNumber = displayNumber, _xpProcess = process}

allocateDisplayNumber :: (MonadUnliftIO m) => m (Maybe DisplayNumber)
allocateDisplayNumber =
  fmap DisplayNumber <$> findM (DisplayNumber >>> xFileDoesNotExist) [0 .. 199]

xFileDoesNotExist :: (MonadUnliftIO m) => DisplayNumber -> m Bool
xFileDoesNotExist (DisplayNumber d) = do
  let filename = "/tmp/.X11-unix/X" <> show d
  liftIO $ not <$> doesFileExist filename
