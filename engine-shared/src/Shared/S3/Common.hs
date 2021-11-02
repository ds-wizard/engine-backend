module Shared.S3.Common where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.UUID as U
import Network.Minio

import LensesConfig
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Util.Logger

createS3Client serverConfig manager = do
  let connectionInfo =
        setCreds
          (Credentials (T.pack $ serverConfig ^. username) (T.pack $ serverConfig ^. password))
          (S.fromString $ serverConfig ^. url)
  liftIO $ mkMinioConn connectionInfo manager

createGetObjectFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> m BS.ByteString
createGetObjectFn object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfo _CMP_S3 (f' "Get object: '%s'" [sanitizedObject])
  let action = do
        src <- getObject (T.pack bucketName) (T.pack sanitizedObject) defaultGetObjectOptions
        let srcStream = gorObjectStream src :: C.ConduitM () BS.ByteString Minio ()
        let foldFn = CL.fold BS.append "" :: C.ConduitT BS.ByteString C.Void Minio BS.ByteString
        C.connect (gorObjectStream src) foldFn
  runMinioClient action

createPutObjectFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> BS.ByteString
  -> m ()
createPutObjectFn object content = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfo _CMP_S3 (f' "Put object: '%s'" [sanitizedObject])
  let req = C.yield content
  let kb15 = 15 * 1024
  let action = putObject (T.pack bucketName) (T.pack sanitizedObject) req (Just kb15) defaultPutObjectOptions
  runMinioClient action

createRemoveObjectFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> m ()
createRemoveObjectFn object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfo _CMP_S3 (f' "Delete object: '%s'" [sanitizedObject])
  let action = removeObject (T.pack bucketName) (T.pack sanitizedObject)
  runMinioClient action

createListObjectsFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m [String]
createListObjectsFn = do
  bucketName <- getBucketName
  logInfo _CMP_S3 "List objects"
  let action = do
        let itemStream = listObjects (T.pack bucketName) Nothing True :: C.ConduitM () ListItem Minio ()
        let foldFn = CL.fold (\acc i -> acc ++ [i]) [] :: C.ConduitT ListItem C.Void Minio [ListItem]
        C.connect itemStream foldFn
  items <- runMinioClient action
  return $ mapMaybe getName items
  where
    getName (ListItemObject objectInfo) = Just . T.unpack . oiObject $ objectInfo
    getName _ = Nothing

createBucketExistsFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m Bool
createBucketExistsFn = do
  bucketName <- getBucketName
  logInfo _CMP_S3 (f' "Check existence of bucket: '%s'" [bucketName])
  let action = bucketExists (T.pack bucketName)
  runMinioClient action

createMakeBucketFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
createMakeBucketFn = do
  bucketName <- getBucketName
  logInfo _CMP_S3 (f' "Make bucket: '%s'" [bucketName])
  let action = makeBucket (T.pack bucketName) Nothing
  runMinioClient action

createPurgeBucketFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
createPurgeBucketFn = do
  bucketName <- getBucketName
  logInfo _CMP_S3 (f' "Purge bucket: '%s'" [bucketName])
  objects <- createListObjectsFn
  traverse_ createRemoveObjectFn objects

createRemoveBucketFn ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
createRemoveBucketFn = do
  bucketName <- getBucketName
  logInfo _CMP_S3 (f' "Remove bucket: '%s'" [bucketName])
  let action = removeBucket (T.pack bucketName)
  runMinioClient action

runMinioClient ::
     ( MonadReader s m
     , HasS3Client' s
     , HasAppUuid' s
     , HasServerConfig' s sc
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => Minio a
  -> m a
runMinioClient action = do
  context <- ask
  let s3Client = context ^. s3Client'
  res <- liftIO $ runMinioWith s3Client action
  case res of
    Left e -> do
      logInfo _CMP_S3 (show e)
      throwError $ GeneralServerError ("Error in s3 connection: " ++ show e)
    Right e -> return e

getBucketName :: (MonadReader s m, HasAppUuid' s, HasServerConfig' s sc, MonadIO m, MonadError AppError m) => m String
getBucketName = do
  context <- ask
  return $ context ^. serverConfig' . s3' . bucket

sanitizeObject ::
     (MonadReader s m, HasAppUuid' s, HasServerConfig' s sc, MonadIO m, MonadError AppError m) => String -> m String
sanitizeObject object = do
  context <- ask
  if context ^. serverConfig' . experimental' . moreAppsEnabled
    then return $ U.toString (context ^. appUuid') ++ "/" ++ object
    else return object
