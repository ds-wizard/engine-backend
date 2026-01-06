module Wizard.S3.Project.ProjectFileS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.UUID as U
import Network.Minio

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "project-files"

retrieveFile :: U.UUID -> U.UUID -> AppContextM BS.ByteString
retrieveFile projectUuid fileUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

retrieveFileConduitAction :: U.UUID -> U.UUID -> AppContextM (Minio (C.ConduitM () BS.ByteString Minio ()))
retrieveFileConduitAction projectUuid fileUuid = createGetObjectConduitActionFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

putFile :: U.UUID -> U.UUID -> String -> BS.ByteString -> AppContextM String
putFile projectUuid fileUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid]) (Just contentType) Nothing

putFileConduit :: U.UUID -> U.UUID -> String -> String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> AppContextM String
putFileConduit projectUuid fileUuid contentType contentDisposition = createPutObjectConduitFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid]) (Just contentType) (Just contentDisposition)

presignGetFileUrl :: U.UUID -> U.UUID -> Int -> AppContextM String
presignGetFileUrl projectUuid fileUuid = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

removeFiles :: U.UUID -> AppContextM ()
removeFiles projectUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString projectUuid])

removeFile :: U.UUID -> U.UUID -> AppContextM ()
removeFile projectUuid fileUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString projectUuid, U.toString fileUuid])

makeBucket :: AppContextM ()
makeBucket = createMakeBucketFn

purgeBucket :: AppContextM ()
purgeBucket = createPurgeBucketFn

removeBucket :: AppContextM ()
removeBucket = createRemoveBucketFn
