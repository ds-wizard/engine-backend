module Wizard.S3.Questionnaire.QuestionnaireFileS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.UUID as U
import Network.Minio

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "questionnaire-files"

retrieveFile :: U.UUID -> U.UUID -> AppContextM BS.ByteString
retrieveFile questionnaireUuid fileUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString questionnaireUuid, U.toString fileUuid])

retrieveFileConduitAction :: U.UUID -> U.UUID -> AppContextM (Minio (C.ConduitM () BS.ByteString Minio ()))
retrieveFileConduitAction questionnaireUuid fileUuid = createGetObjectConduitActionFn (f' "%s/%s/%s" [folderName, U.toString questionnaireUuid, U.toString fileUuid])

putFile :: U.UUID -> U.UUID -> String -> BS.ByteString -> AppContextM String
putFile questionnaireUuid fileUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString questionnaireUuid, U.toString fileUuid]) (Just contentType) Nothing

putFileConduit :: U.UUID -> U.UUID -> String -> String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> AppContextM String
putFileConduit questionnaireUuid fileUuid contentType contentDisposition = createPutObjectConduitFn (f' "%s/%s/%s" [folderName, U.toString questionnaireUuid, U.toString fileUuid]) (Just contentType) (Just contentDisposition)

presigneGetFileUrl :: U.UUID -> U.UUID -> Int -> AppContextM String
presigneGetFileUrl questionnaireUuid fileUuid = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString questionnaireUuid, U.toString fileUuid])

removeFiles :: U.UUID -> AppContextM ()
removeFiles questionnaireUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString questionnaireUuid])

removeFile :: U.UUID -> U.UUID -> AppContextM ()
removeFile questionnaireUuid fileUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString questionnaireUuid, U.toString fileUuid])

makeBucket :: AppContextM ()
makeBucket = createMakeBucketFn

purgeBucket :: AppContextM ()
purgeBucket = createPurgeBucketFn

removeBucket :: AppContextM ()
removeBucket = createRemoveBucketFn
