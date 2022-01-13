module Mortred.Selenium
  ( startSelenium,
    startSeleniumOnPort,
  )
where

import qualified Codec.Archive.Zip.Conduit.UnZip as UnZip
import Conduit
import Control.Lens.Combinators
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as DataText
import qualified Data.Text.IO as TIO
import Mortred.Types
import Mortred.Types.Errors
import Network.HTTP.Client (Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.TLS (getGlobalManager)
import Numeric (readHex)
import Qtility
import qualified RIO.ByteString.Lazy as LazyByteString
import RIO.Directory
  ( Permissions (..),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    emptyPermissions,
    listDirectory,
    setPermissions,
  )
import RIO.FilePath (dropFileName, (</>))
import qualified RIO.FilePath as FilePath
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import qualified RIO.Text as Text
import System.Environment (getEnv)
import System.IO (putStrLn)
import System.Process.Typed

-- | Starts a Selenium server backed by a given `Xvfb` process. The `SeleniumPath` parameter should
-- point to the actual JAR-file to run. Throws `SeleniumStartError` if the Selenium server cannot be
-- started.
startSelenium ::
  (MonadThrow m, MonadUnliftIO m, PrimMonad m, MonadFail m) =>
  XvfbProcess ->
  SeleniumPath ->
  m SeleniumProcess
startSelenium xvfbProcess seleniumPath = do
  port <- allocateServerPort
  startSeleniumOnPort port xvfbProcess seleniumPath

startSeleniumOnPort ::
  (MonadThrow m, MonadUnliftIO m, PrimMonad m, MonadFail m) =>
  SeleniumPort ->
  XvfbProcess ->
  SeleniumPath ->
  m SeleniumProcess
startSeleniumOnPort port xvfbProcess seleniumPath@(SeleniumPath sp) = do
  let chromeDriverPath = FilePath.takeDirectory sp </> "chromedriver"
  chromeDriverExists <- doesChromeDriverExist seleniumPath
  seleniumExists <- liftIO $ doesFileExist sp
  if seleniumExists
    then do
      unless chromeDriverExists $ do
        liftIO $ putStrLn "Downloading `chromedriver`"
        downloadChromeDriver chromeDriverPath
      chromeDriverVersion@(ChromeDriverVersion cdVersion) <-
        getChromeDriverVersion chromeDriverPath
      chromeBinary <-
        fromMaybeM (SeleniumStartChromeDriverError ChromeBinaryNotFound) $
          searchPathForBinary "google-chrome"
      chromeVersion@(ChromeVersion cVersion) <- getChromeVersion chromeBinary
      when (cdVersion /= cVersion) $ do
        throwM $ VersionMismatch chromeVersion chromeDriverVersion
      let processConfiguration =
            proc "java" ["-jar", sp, "-port", port ^. unwrap . unwrap & show]
              & setStdin nullStream
              & setStdout nullStream
              & setStderr nullStream
              & setEnv
                [ ( "DISPLAY",
                    xvfbProcess ^. xpDisplayNumber . unwrap & show & (":" <>)
                  )
                ]
      process <- startProcess processConfiguration
      pure $
        SeleniumProcess
          { _spXvfbProcess = xvfbProcess,
            _spProcess = process,
            _spPort = port
          }
    else throwM $ SeleniumNotFound seleniumPath

downloadChromeDriver ::
  (MonadThrow m, MonadUnliftIO m, PrimMonad m, MonadFail m) =>
  FilePath ->
  m ()
downloadChromeDriver path = do
  manager <- liftIO getGlobalManager
  chromeBinary <- fromMaybeM ChromeBinaryNotFound $ searchPathForBinary "google-chrome"
  chromeVersion <- getChromeVersion chromeBinary
  chromeDriverLink <-
    fromEither $ note (NoValidChromeDriverUrl chromeVersion) $ getChromeDriverLink chromeVersion
  request <- chromeDriverLink ^. unwrap & parseRequest
  response <- mapExceptionM (DownloadError chromeDriverLink) $ liftIO $ httpLbs request manager
  mapExceptionM UnzipError $ unzipIntoPath path $ responseBody response
  liftIO $ setChromeDriverPermissions path

setChromeDriverPermissions :: FilePath -> IO ()
setChromeDriverPermissions path = do
  setPermissions
    path
    ( emptyPermissions
        { writable = True,
          readable = True,
          executable = True
        }
    )

allocateServerPort :: (MonadUnliftIO m) => m SeleniumPort
allocateServerPort = do
  procNetTcpContents <- readFileUtf8 "/proc/net/tcp"
  procNetTcp6Contents <- readFileUtf8 "/proc/net/tcp6"
  let lineToPort l =
        case l & Text.strip & Text.words & take 2 & drop 1 & fmap (Text.split (== ':')) of
          [[_localAddress, portText]] ->
            portText & Text.unpack & readHex & Partial.head & fst & PortNumber
          _other ->
            error $ "Bad line in '/proc/net/tcp': '" <> show l <> "'"
      takenPorts = procNetTcpContents & Text.lines & drop 1 & map lineToPort
      takenIPV6Ports = procNetTcp6Contents & Text.lines & drop 1 & map lineToPort
      allocate' p = if isTaken p then allocate' (p & unwrap %~ (+ 1)) else p
      isTaken = (`elem` (takenPorts <> takenIPV6Ports))
  pure $ SeleniumPort $ allocate' $ PortNumber 4444

unzipIntoPath ::
  (MonadThrow m, MonadUnliftIO m, PrimMonad m, MonadFail m) =>
  FilePath ->
  LByteString ->
  m ()
unzipIntoPath path content =
  void $ runConduitRes $ CB.sourceLbs content .| fuseUpstream UnZip.unZipStream (extract path)

getBinaryVersionText :: (MonadThrow m, MonadUnliftIO m) => FilePath -> m LByteString
getBinaryVersionText path = do
  let processConfiguration =
        proc path ["--version"]
          & setStdout byteStringOutput
          & setStdin nullStream
          & setStderr nullStream
  (^. _2) <$> mapExceptionM (UnableToReadChromeProcess path) (readProcessStdout processConfiguration)

getChromeVersion :: (MonadThrow m, MonadUnliftIO m) => FilePath -> m ChromeVersion
getChromeVersion = getBinaryVersion 2 ChromeVersion BadChromeVersionOutput

getChromeDriverVersion :: (MonadThrow m, MonadUnliftIO m) => FilePath -> m ChromeDriverVersion
getChromeDriverVersion = getBinaryVersion 1 ChromeDriverVersion BadChromeDriverVersionOutput

getBinaryVersion ::
  (MonadThrow m, MonadUnliftIO m, Exception e) =>
  Int ->
  (MajorVersion -> v) ->
  (LByteString -> e) ->
  FilePath ->
  m v
getBinaryVersion outputIndex versionConstructor e path = do
  output <- liftIO $ getBinaryVersionText path
  splitOnSpaces output ^? ix outputIndex
    & maybe
      (throwM $ e output)
      (constructMajorVersion >>> fmap versionConstructor)

textToMajorVersion :: Text -> Maybe MajorVersion
textToMajorVersion "93" = Just $ MajorVersion 93
textToMajorVersion "94" = Just $ MajorVersion 94
textToMajorVersion "95" = Just $ MajorVersion 95
textToMajorVersion "96" = Just $ MajorVersion 96
textToMajorVersion "97" = Just $ MajorVersion 97
textToMajorVersion _ = Nothing

doesChromeDriverExist :: (MonadUnliftIO m) => SeleniumPath -> m Bool
doesChromeDriverExist (SeleniumPath sp) = do
  let basePath = dropFileName sp
  inSeleniumPath <- doesFileExist (basePath </> "chromedriver")
  if inSeleniumPath then pure inSeleniumPath else isJust <$> searchPathForBinary "chromedriver"

searchPathForBinary :: (MonadUnliftIO m) => FilePath -> m (Maybe FilePath)
searchPathForBinary binary = do
  pathContents <- (Text.pack >>> Text.split (== ':')) <$> liftIO (getEnv "PATH")
  let getBinariesForPath p = do
        directoryContents <- listDirectory p
        files <- filterM ((p </>) >>> doesFileExist) directoryContents
        pure (p, files)
  binaries <- filterM doesDirectoryExist (Text.unpack <$> pathContents) >>= mapM getBinariesForPath
  let maybeFoundInPath = List.find (\(_path, binaryNames) -> binary `elem` binaryNames) binaries
  pure $ (fst >>> (</> binary)) <$> maybeFoundInPath

-- Lifted from https://github.com/dylex/zip-stream/blob/master/cmd/unzip.hs
-- See `licenses/zip-stream.txt` for license information.
extract ::
  (MonadIO m, MonadFail m) =>
  FilePath ->
  ConduitT (Either UnZip.ZipEntry ByteString) Void m ()
extract path = awaitForever start
  where
    start (Left UnZip.ZipEntry {..}) = do
      liftIO $ either TIO.putStrLn Char8.putStrLn zipEntryName
      liftIO $ createDirectoryIfMissing True (FilePath.takeDirectory fullName)
      if either DataText.last Char8.last zipEntryName == '/'
        then when ((0 /=) `any` zipEntrySize) $ fail $ fullName ++ ": non-empty directory"
        else do
          h <- liftIO $ openFile fullName WriteMode
          mapM_ (liftIO . hSetFileSize h . toInteger) zipEntrySize
          write .| CB.sinkHandle h
          liftIO $ hClose h
      where
        name =
          either
            (Text.unpack . Text.dropWhile ('/' ==))
            (Char8.unpack . Char8.dropWhile ('/' ==))
            zipEntryName
        fullName = FilePath.takeDirectory path </> name
    start (Right _) = fail "Unexpected leading or directory data contents"
    write = await >>= maybe (return ()) block
    block (Right b) = yield b >> write
    block a = leftover a

chromeDriverLinks :: [(ChromeVersion, Url)]
chromeDriverLinks =
  [ ( ChromeVersion $ MajorVersion 97,
      Url "https://chromedriver.storage.googleapis.com/97.0.4692.36/chromedriver_linux64.zip"
    ),
    ( ChromeVersion $ MajorVersion 96,
      Url "https://chromedriver.storage.googleapis.com/96.0.4664.45/chromedriver_linux64.zip"
    ),
    ( ChromeVersion $ MajorVersion 95,
      Url "https://chromedriver.storage.googleapis.com/95.0.4638.17/chromedriver_linux64.zip"
    ),
    ( ChromeVersion $ MajorVersion 94,
      Url "https://chromedriver.storage.googleapis.com/94.0.4606.61/chromedriver_linux64.zip"
    ),
    ( ChromeVersion $ MajorVersion 93,
      Url "https://chromedriver.storage.googleapis.com/93.0.4577.63/chromedriver_linux64.zip"
    )
  ]

getChromeDriverLink :: ChromeVersion -> Maybe Url
getChromeDriverLink chromeVersion = snd <$> List.find (fst >>> (== chromeVersion)) chromeDriverLinks

constructMajorVersion :: (MonadThrow m) => Text -> m MajorVersion
constructMajorVersion version =
  version
    & Text.takeWhile (/= '.')
    & textToMajorVersion
    & fromPureMaybeM (UnsupportedMajorVersion version)

splitOnSpaces :: LByteString -> [Text]
splitOnSpaces = LazyByteString.toStrict >>> decodeUtf8Lenient >>> Text.strip >>> Text.split (== ' ')
