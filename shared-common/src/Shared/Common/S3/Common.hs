module Shared.Common.S3.Common where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, liftIO)
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
import Network.Minio
import Network.Minio.S3API

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.JSON
import Shared.Common.Util.Logger

createS3Client serverConfig manager = do
  let connectionInfo =
        setCreds
          (CredentialValue (S.fromString serverConfig.username) (S.fromString serverConfig.password) Nothing)
          (S.fromString serverConfig.url)
  let connectionInfoWithRegion =
        case serverConfig.region of
          Just reg -> setRegion (T.pack reg) connectionInfo
          Nothing -> connectionInfo
  liftIO $ mkMinioConn connectionInfoWithRegion manager

createGetObjectFn :: AppContextC s sc m => String -> m BS.ByteString
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

createGetObjectConduitActionFn :: AppContextC s sc m => String -> m (Minio (C.ConduitM () BS.ByteString Minio ()))
createGetObjectConduitActionFn object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Get object: '%s'" [sanitizedObject])
  let action = do
        src <- getObject (T.pack bucketName) (T.pack sanitizedObject) defaultGetObjectOptions
        return $ gorObjectStream src
  return action

createGetObjectWithTenantFn :: AppContextC s sc m => U.UUID -> String -> m BS.ByteString
createGetObjectWithTenantFn tenantUuid object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObjectWithTenant tenantUuid object
  logInfoI _CMP_S3 (f' "Get object: '%s'" [sanitizedObject])
  let action = do
        src <- getObject (T.pack bucketName) (T.pack sanitizedObject) defaultGetObjectOptions
        let srcStream = gorObjectStream src :: C.ConduitM () BS.ByteString Minio ()
        let foldFn = CL.fold BS.append "" :: C.ConduitT BS.ByteString C.Void Minio BS.ByteString
        C.connect (gorObjectStream src) foldFn
  runMinioClient action

createGetObjectFn' :: AppContextC s sc m => String -> m (Either MinioErr BS.ByteString)
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

createPutObjectFn :: AppContextC s sc m => String -> Maybe String -> Maybe String -> BS.ByteString -> m String
createPutObjectFn object mContentType mContentDisposition content = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Put object: '%s'" [sanitizedObject])
  let req = C.yield content
  let kb15 = 15 * 1024
  let objectOptions =
        defaultPutObjectOptions
          { pooContentType = fmap T.pack mContentType
          , pooContentDisposition = fmap T.pack mContentDisposition
          }
  let action = putObject (T.pack bucketName) (T.pack sanitizedObject) req (Just kb15) objectOptions
  runMinioClient action
  buildObjectUrl sanitizedObject

createPutObjectWithTenantFn :: AppContextC s sc m => U.UUID -> String -> Maybe String -> Maybe String -> BS.ByteString -> m String
createPutObjectWithTenantFn tenantUuid object mContentType mContentDisposition content = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObjectWithTenant tenantUuid object
  logInfoI _CMP_S3 (f' "Put object: '%s'" [sanitizedObject])
  let req = C.yield content
  let kb15 = 15 * 1024
  let objectOptions =
        defaultPutObjectOptions
          { pooContentType = fmap T.pack mContentType
          , pooContentDisposition = fmap T.pack mContentDisposition
          }
  let action = putObject (T.pack bucketName) (T.pack sanitizedObject) req (Just kb15) objectOptions
  runMinioClient action
  buildObjectUrl sanitizedObject

createPutObjectConduitFn :: AppContextC s sc m => String -> Maybe String -> Maybe String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> m String
createPutObjectConduitFn object mContentType mContentDisposition getAction = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Put object: '%s'" [sanitizedObject])
  let kb15 = 15 * 1024
  let objectOptions =
        defaultPutObjectOptions
          { pooContentType = fmap T.pack mContentType
          , pooContentDisposition = fmap T.pack mContentDisposition
          }
  let action = do
        object <- getAction
        putObject (T.pack bucketName) (T.pack sanitizedObject) object (Just kb15) objectOptions
  runMinioClient action
  buildObjectUrl sanitizedObject

createRemoveObjectFn :: AppContextC s sc m => String -> m ()
createRemoveObjectFn object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Delete object: '%s'" [sanitizedObject])
  let action = removeObject (T.pack bucketName) (T.pack sanitizedObject)
  runMinioClient action

createRemoveObjectWithTenantFn :: AppContextC s sc m => U.UUID -> String -> m ()
createRemoveObjectWithTenantFn tenantUuid object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObjectWithTenant tenantUuid object
  logInfoI _CMP_S3 (f' "Delete object: '%s'" [sanitizedObject])
  let action = removeObject (T.pack bucketName) (T.pack sanitizedObject)
  runMinioClient action

createListObjectsFn :: AppContextC s sc m => m [String]
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

createBucketExistsFn :: AppContextC s sc m => m Bool
createBucketExistsFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Check existence of bucket: '%s'" [bucketName])
  let action = bucketExists (T.pack bucketName)
  runMinioClient action

createMakeBucketFn :: AppContextC s sc m => m ()
createMakeBucketFn = do
  exists <- createBucketExistsFn
  unless exists $ do
    bucketName <- getBucketName
    logInfoI _CMP_S3 (f' "Make bucket: '%s'" [bucketName])
    let action = makeBucket (T.pack bucketName) Nothing
    runMinioClient action

createPurgeBucketFn :: AppContextC s sc m => m ()
createPurgeBucketFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Purge bucket: '%s'" [bucketName])
  objects <- createListObjectsFn
  traverse_ createRemoveObjectFn objects

createRemoveBucketFn :: AppContextC s sc m => m ()
createRemoveBucketFn = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Remove bucket: '%s'" [bucketName])
  let action = removeBucket (T.pack bucketName)
  runMinioClient action

createSetBucketPolicyFn :: AppContextC s sc m => String -> String -> m ()
createSetBucketPolicyFn prefix policy = do
  bucketName <- getBucketName
  logInfoI _CMP_S3 (f' "Set policy: '%s' with '%s'" [prefix, policy])
  let action =
        setBucketPolicy
          (T.pack bucketName)
          "{\"bucketName\": \"wizard-server\", \"prefix\": \"configs/logo.png\", \"policy\": \"readonly\"}"
  runMinioClient action

makeBucketPublicReadOnly :: AppContextC s sc m => m ()
makeBucketPublicReadOnly = do
  bucketName <- getBucketName
  context <- ask
  let resource =
        if context.serverConfig'.cloud'.enabled
          then f' "arn:aws:s3:::%s/%s/public/*" [bucketName, U.toString context.tenantUuid']
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

createPresignedGetObjectUrl :: AppContextC s sc m => String -> Int -> m String
createPresignedGetObjectUrl object expirationInSeconds = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  logInfoI _CMP_S3 (f' "Presign get object url: '%s'" [sanitizedObject])
  let action = presignedGetObjectUrl (T.pack bucketName) (T.pack sanitizedObject) expirationInSeconds [] []
  result <- runMinioClient action
  return $ BS.unpack result

createMakePublicLink :: AppContextC s sc m => String -> m String
createMakePublicLink object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObject object
  url <- getS3Url
  let publicLink = f' "%s/%s/%s" [url, bucketName, sanitizedObject]
  logInfoI _CMP_S3 (f' "Public URL to share: '%s'" [publicLink])
  return publicLink

createMakePublicLinkWithTenantFn :: AppContextC s sc m => U.UUID -> String -> m String
createMakePublicLinkWithTenantFn tenantUuid object = do
  bucketName <- getBucketName
  sanitizedObject <- sanitizeObjectWithTenant tenantUuid object
  url <- getS3Url
  let publicLink = f' "%s/%s/%s" [url, bucketName, sanitizedObject]
  logInfoI _CMP_S3 (f' "Public URL to share: '%s'" [publicLink])
  return publicLink

runMinioClient :: AppContextC s sc m => Minio a -> m a
runMinioClient action = do
  context <- ask
  let s3Client = context.s3Client'
  res <- liftIO $ runMinioWith s3Client action
  case res of
    Left e -> do
      logInfoI _CMP_S3 (show e)
      throwError . GeneralServerError $ _ERROR_S3__GENERIC_ERROR (show e)
    Right e -> return e

runMinioClient' :: AppContextC s sc m => Minio a -> m (Either MinioErr a)
runMinioClient' action = do
  context <- ask
  let s3Client = context.s3Client'
  liftIO $ runMinioWith s3Client action

getS3Url :: AppContextC s sc m => m String
getS3Url = do
  context <- ask
  return context.serverConfig'.s3'.url

getBucketName :: AppContextC s sc m => m String
getBucketName = do
  context <- ask
  return context.serverConfig'.s3'.bucket

sanitizeObject :: AppContextC s sc m => String -> m String
sanitizeObject object = do
  context <- ask
  if context.serverConfig'.cloud'.enabled && not (U.toString context.tenantUuid' `L.isPrefixOf` object)
    then return $ U.toString context.tenantUuid' ++ "/" ++ object
    else return object

sanitizeObjectWithTenant :: AppContextC s sc m => U.UUID -> String -> m String
sanitizeObjectWithTenant tenantUuid object = do
  context <- ask
  if context.serverConfig'.cloud'.enabled && not (U.toString tenantUuid `L.isPrefixOf` object)
    then return $ U.toString tenantUuid ++ "/" ++ object
    else return object

buildObjectUrl :: AppContextC s sc m => String -> m String
buildObjectUrl object = do
  s3Url <- getS3Url
  bucketName <- getBucketName
  return $ f' "%s/%s/%s" [s3Url, bucketName, object]
