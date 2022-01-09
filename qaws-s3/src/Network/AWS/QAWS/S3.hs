-- | Has utility functions for dealing with Amazon's Simple Storage Service (S3).
module Network.AWS.QAWS.S3
  ( getFileStream,
    runWithFileStream,
    runWithFileStream',
    objectExists,
    objectExists',
    putObject,
    putObject',
    putJSON,
    putJSON',
    listObjects,
    listObjects',
  )
where

import Conduit (ConduitT, ResourceT, runConduitRes, (.|))
import Control.Lens ((?~))
import Control.Lens.Prism (_Just)
import Data.Aeson (ToJSON (..), encode)
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Body as AWS
import Network.AWS.QAWS
import Network.AWS.QAWS.S3.Types
import qualified Network.AWS.S3 as AWSS3
import Qtility.Data (tryAs, unwrap)
import RIO
import qualified RIO.HashMap as HashMap

objectExists ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env, Exception e, AWS.AsError e) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  m (Either e Bool)
objectExists bucketName objectKey = do
  awsEnv <- view AWS.environment
  objectExists' awsEnv bucketName objectKey

objectExists' ::
  (MonadUnliftIO m, MonadThrow m, Exception e, AWS.AsError e) =>
  AWS.Env ->
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  m (Either e Bool)
objectExists' awsEnv bucket key = do
  let command = AWSS3.headObject bucket key
  either Left (((^. AWSS3.horsResponseStatus) >>> (== 200)) >>> Right) <$> tryRunAWS' awsEnv command

getFileStream ::
  (AWS.MonadAWS m) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  m (ConduitT () ByteString (ResourceT IO) ())
getFileStream bucket key = do
  let command = AWSS3.getObject bucket key
  ((^. AWSS3.gorsBody) >>> AWS._streamBody) <$> AWS.send command

runWithFileStream ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env, Exception e, AWS.AsError e) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  ConduitT ByteString Void (ResourceT IO) a ->
  m (Either e a)
runWithFileStream bucket key downstream = do
  awsEnv <- view AWS.environment
  runWithFileStream' awsEnv bucket key downstream

runWithFileStream' ::
  (MonadUnliftIO m, MonadThrow m, Exception e, AWS.AsError e) =>
  AWS.Env ->
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  ConduitT ByteString Void (ResourceT IO) a ->
  m (Either e a)
runWithFileStream' awsEnv bucket key downstream = do
  tryAs AWS._Error $
    AWS.runResourceT $
      AWS.runAWS awsEnv $ do
        fileStream <- getFileStream bucket key
        liftIO $ runConduitRes $ fileStream .| downstream

putObject ::
  ( MonadUnliftIO m,
    MonadThrow m,
    MonadReader env m,
    AWS.HasEnv env,
    AWS.ToBody a,
    Exception e,
    AWS.AsError e
  ) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  a ->
  m (Either e ())
putObject bucket key a = do
  awsEnv <- view AWS.environment
  putObject' awsEnv bucket key a

putObject' ::
  (MonadUnliftIO m, MonadThrow m, AWS.ToBody a, Exception e, AWS.AsError e) =>
  AWS.Env ->
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  a ->
  m (Either e ())
putObject' awsEnv bucket key a = do
  let command = AWSS3.putObject bucket key (AWS.toBody a)
  void <$> tryRunAWS' awsEnv command

putJSON ::
  ( MonadUnliftIO m,
    MonadThrow m,
    MonadReader env m,
    AWS.HasEnv env,
    ToJSON a,
    Exception e,
    AWS.AsError e
  ) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  a ->
  m (Either e ())
putJSON bucket key a = do
  awsEnv <- view AWS.environment
  putJSON' awsEnv bucket key a

putJSON' ::
  (MonadUnliftIO m, MonadThrow m, ToJSON a, Exception e, AWS.AsError e) =>
  AWS.Env ->
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  a ->
  m (Either e ())
putJSON' awsEnv bucket key a = do
  let command =
        AWSS3.putObject bucket key (AWS.toBody (encode a))
          & AWSS3.poContentType ?~ "application/json"
          & AWSS3.poMetadata .~ HashMap.fromList [("Content-Type", "application/json")]
  void <$> tryRunAWS' awsEnv command

listObjects ::
  (MonadUnliftIO m, MonadThrow m, MonadReader env m, AWS.HasEnv env, Exception e, AWS.AsError e) =>
  AWSS3.BucketName ->
  ListObjectOptions ->
  m (Either e ([AWSS3.Object], Maybe ContinuationToken))
listObjects bucket listObjectOptions = do
  awsEnv <- view AWS.environment
  listObjects' awsEnv bucket listObjectOptions

listObjects' ::
  (MonadUnliftIO m, MonadThrow m, Exception e, AWS.AsError e) =>
  AWS.Env ->
  AWSS3.BucketName ->
  ListObjectOptions ->
  m (Either e ([AWSS3.Object], Maybe ContinuationToken))
listObjects' awsEnv bucket listOptions = do
  let command =
        AWSS3.listObjectsV2 bucket
          & AWSS3.lovContinuationToken .~ listOptions ^? looContinuationToken . _Just . unwrap
          & AWSS3.lovPrefix .~ listOptions ^? looKeyPrefix . _Just . unwrap
          & AWSS3.lovMaxKeys .~ listOptions ^? looMaxKeys . _Just . unwrap
          & AWSS3.lovStartAfter .~ listOptions ^? looStartAfter . _Just . unwrap
  either Left (extractValues >>> Right) <$> tryRunAWS' awsEnv command
  where
    extractValues r = do
      let objects = r ^. AWSS3.lovrsContents
          continuationToken = ContinuationToken <$> r ^. AWSS3.lovrsContinuationToken
      (objects, continuationToken)
