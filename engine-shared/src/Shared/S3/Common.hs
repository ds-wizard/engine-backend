module Shared.S3.Common where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as U
import GHC.Records
import Network.Minio
import Network.Minio.S3API

import Shared.Localization.Messages.Internal
import Shared.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Util.JSON
import Shared.Util.Logger

createS3Client serverConfig manager = do
  let connectionInfo =
        setCreds
          (Credentials (T.pack serverConfig.username) (T.pack serverConfig.password))
          (S.fromString serverConfig.url)
  let connectionInfoWithRegion =
        case serverConfig.region of
          Just reg -> setRegion (T.pack reg) connectionInfo
          Nothing -> connectionInfo
  liftIO $ mkMinioConn connectionInfoWithRegion manager

createGetObjectFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> m BS.ByteString
createGetObjectFn object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Get object: '%s'" [sanitizedObject])
  let action = do
        src <- getObject (T.pack bucketName) (T.pack sanitizedObject) defaultGetObjectOptions
        let srcStream = gorObjectStream src :: C.ConduitM () BS.ByteString Minio ()
        let foldFn = CL.fold BS.append "" :: C.ConduitT BS.ByteString C.Void Minio BS.ByteString
        C.connect (gorObjectStream src) foldFn
  runMinioClient action

createGetObjectWithAppFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => U.UUID
  -> String
  -> m BS.ByteString
createGetObjectWithAppFn appUuid object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObjectWithApp appUuid object
  logInfoI _CMP_S3 (f' "Get object: '%s'" [sanitizedObject])
  let action = do
        src <- getObject (T.pack bucketName) (T.pack sanitizedObject) defaultGetObjectOptions
        let srcStream = gorObjectStream src :: C.ConduitM () BS.ByteString Minio ()
        let foldFn = CL.fold BS.append "" :: C.ConduitT BS.ByteString C.Void Minio BS.ByteString
        C.connect (gorObjectStream src) foldFn
  runMinioClient action

createGetObjectFn'
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> m (Either MinioErr BS.ByteString)
createGetObjectFn' object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Get object: '%s'" [sanitizedObject])
  let action = do
        src <- getObject (T.pack bucketName) (T.pack sanitizedObject) defaultGetObjectOptions
        let srcStream = gorObjectStream src :: C.ConduitM () BS.ByteString Minio ()
        let foldFn = CL.fold BS.append "" :: C.ConduitT BS.ByteString C.Void Minio BS.ByteString
        C.connect (gorObjectStream src) foldFn
  runMinioClient' action

createPutObjectFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> Maybe String
  -> BS.ByteString
  -> m String
createPutObjectFn object mContentType content = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Put object: '%s'" [sanitizedObject])
  let req = C.yield content
  let kb15 = 15 * 1024
  let objectOptions = defaultPutObjectOptions {pooContentType = fmap T.pack mContentType}
  let action = putObject (T.pack bucketName) (T.pack sanitizedObject) req (Just kb15) objectOptions
  runMinioClient action
  buildObjectUrl sanitizedObject

createPutObjectWithAppFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => U.UUID
  -> String
  -> Maybe String
  -> BS.ByteString
  -> m String
createPutObjectWithAppFn appUuid object mContentType content = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObjectWithApp appUuid object
  logInfoI _CMP_S3 (f' "Put object: '%s'" [sanitizedObject])
  let req = C.yield content
  let kb15 = 15 * 1024
  let objectOptions = defaultPutObjectOptions {pooContentType = fmap T.pack mContentType}
  let action = putObject (T.pack bucketName) (T.pack sanitizedObject) req (Just kb15) objectOptions
  runMinioClient action
  buildObjectUrl sanitizedObject

createRemoveObjectFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> m ()
createRemoveObjectFn object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Delete object: '%s'" [sanitizedObject])
  let action = removeObject (T.pack bucketName) (T.pack sanitizedObject)
  runMinioClient action

createListObjectsFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m [String]
createListObjectsFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 "List objects"
  let action = do
        let itemStream = listObjects (T.pack bucketName) Nothing True :: C.ConduitM () ListItem Minio ()
        let foldFn = CL.fold (\acc i -> acc ++ [i]) [] :: C.ConduitT ListItem C.Void Minio [ListItem]
        C.connect itemStream foldFn
  items <- runMinioClient action
  return $ mapMaybe getName items
  where
    getName (ListItemObject objectInfo) = Just . T.unpack . oiObject $ objectInfo
    getName _ = Nothing

createBucketExistsFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m Bool
createBucketExistsFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Check existence of bucket: '%s'" [bucketName])
  let action = bucketExists (T.pack bucketName)
  runMinioClient action

createMakeBucketFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
createMakeBucketFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Make bucket: '%s'" [bucketName])
  let action = makeBucket (T.pack bucketName) Nothing
  runMinioClient action

createPurgeBucketFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
createPurgeBucketFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Purge bucket: '%s'" [bucketName])
  objects <- createListObjectsFn
  traverse_ createRemoveObjectFn objects

createRemoveBucketFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
createRemoveBucketFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Remove bucket: '%s'" [bucketName])
  let action = removeBucket (T.pack bucketName)
  runMinioClient action

createSetBucketPolicyFn
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> String
  -> m ()
createSetBucketPolicyFn prefix policy = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Set policy: '%s' with '%s'" [prefix, policy])
  let action =
        setBucketPolicy
          (T.pack bucketName)
          "{\"bucketName\": \"engine-wizard\", \"prefix\": \"configs/logo.png\", \"policy\": \"readonly\"}"
  runMinioClient action

makeBucketPublicReadOnly
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => m ()
makeBucketPublicReadOnly = do
  bucketName <- getBucketName
  context <- ask
  let resource =
        if context.serverConfig'.cloud'.enabled
          then f' "arn:aws:s3:::%s/%s/public/*" [bucketName, U.toString context.appUuid']
          else f' "arn:aws:s3:::%s/public/*" [bucketName]
  logInfoI _CMP_S3 (f' "Make bucket public for read-only access: '%s'" [resource])
  let policy =
        TE.decodeUtf8 . BSL.toStrict . encode $
          obj
            [ ("Version", "2012-10-17")
            ,
              ( "Statement"
              , arr
                  [ obj
                      [ ("Sid", "")
                      , ("Effect", "Allow")
                      , ("Principal", obj [("AWS", arr ["*"])])
                      , ("Action", arr ["s3:GetObject"])
                      , ("Resource", arr [str resource])
                      ]
                  ]
              )
            ]
  let action = setBucketPolicy (T.pack bucketName) policy
  runMinioClient action
  logInfoI _CMP_S3 (f' "Bucket was exposed as public: '%s'" [resource])

createPresignedGetObjectUrl
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> Int
  -> m String
createPresignedGetObjectUrl object expirationInSeconds = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Presign get object url: '%s'" [sanitizedObject])
  let action = presignedGetObjectUrl (T.pack bucketName) (T.pack sanitizedObject) expirationInSeconds [] []
  result <- runMinioClient action
  return . BS.unpack $ result

createMakePublicLink
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> String
  -> m String
createMakePublicLink folderName object = do
  bucketName <- getBucketName
  context <- ask
  publicUrl <- getS3PublicUrl
  let url =
        if context.serverConfig'.cloud'.enabled
          then f' "%s/%s/%s/%s" [publicUrl, U.toString context.appUuid', folderName, object]
          else f' "%s/%s/%s" [publicUrl, folderName, object]
  logInfoI _CMP_S3 (f' "Public URL to share: '%s'" [url])
  return url

runMinioClient
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => Minio a
  -> m a
runMinioClient action = do
  context <- ask
  let s3Client = context.s3Client'
  res <- liftIO $ runMinioWith s3Client action
  case res of
    Left e -> do
      logInfoI _CMP_S3 (show e)
      throwError . GeneralServerError $ _ERROR_S3__GENERIC_ERROR (show e)
    Right e -> return e

runMinioClient'
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "cloud'" sc ServerConfigCloud
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => Minio a
  -> m (Either MinioErr a)
runMinioClient' action = do
  context <- ask
  let s3Client = context.s3Client'
  liftIO $ runMinioWith s3Client action

getS3Url :: (MonadReader s m, HasField "appUuid'" s U.UUID, HasField "serverConfig'" s sc, HasField "s3'" sc ServerConfigS3, MonadIO m, MonadError AppError m) => m String
getS3Url = do
  context <- ask
  return context.serverConfig'.s3'.url

getS3PublicUrl :: (MonadReader s m, HasField "appUuid'" s U.UUID, HasField "serverConfig'" s sc, HasField "cloud'" sc ServerConfigCloud, HasField "s3'" sc ServerConfigS3, MonadIO m, MonadError AppError m) => m String
getS3PublicUrl = do
  context <- ask
  return $
    case context.serverConfig'.s3'.region of
      Just _ -> context.serverConfig'.s3'.publicUrl
      Nothing -> f' "%s/%s" [context.serverConfig'.s3'.publicUrl, context.serverConfig'.s3'.bucket]

getBucketName :: (MonadReader s m, HasField "serverConfig'" s sc, HasField "s3'" sc ServerConfigS3, MonadIO m, MonadError AppError m) => m String
getBucketName = do
  context <- ask
  return context.serverConfig'.s3'.bucket

sanitizeObject
  :: (MonadReader s m, HasField "appUuid'" s U.UUID, HasField "serverConfig'" s sc, HasField "cloud'" sc ServerConfigCloud, MonadIO m, MonadError AppError m) => String -> m String
sanitizeObject object = do
  context <- ask
  if context.serverConfig'.cloud'.enabled && not (U.toString context.appUuid' `L.isPrefixOf` object)
    then return $ U.toString context.appUuid' ++ "/" ++ object
    else return object

sanitizeObjectWithApp
  :: (MonadReader s m, HasField "serverConfig'" s sc, HasField "cloud'" sc ServerConfigCloud, MonadIO m, MonadError AppError m) => U.UUID -> String -> m String
sanitizeObjectWithApp appUuid object = do
  context <- ask
  if context.serverConfig'.cloud'.enabled && not (U.toString appUuid `L.isPrefixOf` object)
    then return $ U.toString appUuid ++ "/" ++ object
    else return object

buildObjectUrl
  :: ( MonadReader s m
     , HasField "s3Client'" s MinioConn
     , HasField "appUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "s3'" sc ServerConfigS3
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     )
  => String
  -> m String
buildObjectUrl object = do
  s3Url <- getS3Url
  bucketName <- getBucketName
  return $ f' "%s/%s/%s" [s3Url, bucketName, object]
