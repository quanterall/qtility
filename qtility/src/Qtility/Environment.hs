module Qtility.Environment
  ( FromEnvironmentValue (..),
    readEnvironmentVariable,
    loadDotEnvFile,
    parseDotEnvFile,
  )
where

import Qtility.Data (PortNumber (..), Url (..), note)
import Qtility.Environment.Types
import Qtility.Time.Types (Seconds (..))
import RIO
import qualified RIO.Char as Char
import qualified RIO.Directory as Directory
import qualified RIO.Text as Text
import System.Environment (lookupEnv, setEnv)

-- | Reads a value from an environment value or returns a string explaining why the value cannot be
-- decoded from the environment key's associated value string. Throws 'ReadEnvironmentVariableError'
-- on error.
readEnvironmentVariable :: (FromEnvironmentValue a, MonadUnliftIO m) => EnvironmentKey -> m a
readEnvironmentVariable key = do
  value <- fromEitherM $ mapLeft ReadEnvironmentMissingValue <$> getEnvironmentValue key
  fromEither $ mapLeft (handleDecodingError key value) $ fromEnvironmentValue value
  where
    handleDecodingError k v = ReadEnvironmentInvalidValue k (EnvironmentValue v)

-- | Loads a @.env@ file if it's available, changing the current environment. Throws
-- 'EnvironmentFileNotFound' if the environment file cannot be found.
loadDotEnvFile :: (MonadThrow m, MonadIO m) => EnvironmentFile -> m ()
loadDotEnvFile ef@(EnvironmentFile path) = do
  unlessM (Directory.doesFileExist path) $ throwM $ EnvironmentFileNotFound ef
  dotEnvValues <- parseDotEnvFile ef
  liftIO $
    forM_ dotEnvValues $ \(key, value) -> do
      -- If there is an environment variable that has the wrong formatting, we'll get an
      -- @IOException@ here. We'll just ignore it and move on.
      setEnv (_unEnvironmentKey key) value `catchIO` const (pure ())

-- | Parses a @.env@ file into a list of key value pairs.
parseDotEnvFile :: (MonadIO m) => EnvironmentFile -> m [(EnvironmentKey, String)]
parseDotEnvFile (EnvironmentFile filePath) = do
  ( Text.lines
      >>> fmap Text.strip
      >>> filter (\l -> l /= "" && not (Text.isPrefixOf "#" l))
      >>> fmap (Text.break (== '='))
      >>> fmap (bimap sanitizeKey sanitizeValue)
    )
    <$> liftIO (readFileUtf8 filePath)
  where
    sanitizeKey = Text.dropWhile (`elem` [' ', '#']) >>> Text.unpack >>> EnvironmentKey
    sanitizeValue = Text.dropWhile (== '=') >>> Text.filter (/= '"') >>> Text.unpack

class FromEnvironmentValue a where
  fromEnvironmentValue :: String -> Either String a

instance FromEnvironmentValue String where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = Right v

instance FromEnvironmentValue Text where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = Right $ fromString v

instance FromEnvironmentValue ByteString where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = Right $ fromString v

instance FromEnvironmentValue Int where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = note ("Unable to read value as `Int`: " <> v) $ readMaybe v

instance FromEnvironmentValue Integer where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = note ("Unable to read value as `Integer`: " <> v) $ readMaybe v

instance FromEnvironmentValue Float where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = note ("Unable to read value as `Float`: " <> v) $ readMaybe v

instance FromEnvironmentValue Double where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = note ("Unable to read value as `Double`: " <> v) $ readMaybe v

instance FromEnvironmentValue Bool where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue s
    | map Char.toLower s `elem` ["yes", "true", "on"] = Right True
    | map Char.toLower s `elem` ["no", "false", "off"] = Right False
    | otherwise = note ("Unable to read value as `Bool`: " <> s) $ readMaybe s

instance FromEnvironmentValue Natural where
  fromEnvironmentValue s = do
    i <- note ("Unable to read value as `Natural`: " <> s) $ readMaybe @Integer s
    if i >= 0 then i & fromIntegral & Right else Left "Value is not a natural number"

instance FromEnvironmentValue PortNumber where
  fromEnvironmentValue s = do
    i <- note ("Unable to read value as `PortNumber`: " <> s) $ readMaybe @Int s
    if i >= 0 && i <= 65535 then i & PortNumber & Right else Left "Value is not a valid port number"

instance FromEnvironmentValue Url where
  fromEnvironmentValue "" = Left "empty value"
  fromEnvironmentValue v = v & Url & Right

instance FromEnvironmentValue a => FromEnvironmentValue (Seconds a) where
  fromEnvironmentValue = fromEnvironmentValue >>> fmap Seconds

getEnvironmentValue :: (MonadIO m) => EnvironmentKey -> m (Either EnvironmentKey String)
getEnvironmentValue key = do
  maybe (Left key) Right <$> liftIO (lookupEnv $ _unEnvironmentKey key)
