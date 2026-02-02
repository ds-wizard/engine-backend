module Registry.S3.DocumentTemplate.DocumentTemplateS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.S3.Common
import Shared.Common.Util.String (f')

folderName = "document-templates"

retrieveAsset :: U.UUID -> U.UUID -> AppContextM BS.ByteString
retrieveAsset documentTemplateUuid assetUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

putAsset :: U.UUID -> U.UUID -> String -> BS.ByteString -> AppContextM String
putAsset documentTemplateUuid assetUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid]) (Just contentType) Nothing

makePublicLink :: U.UUID -> U.UUID -> AppContextM String
makePublicLink documentTemplateUuid assetUuid = createMakePublicLink (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

removeAssets :: U.UUID -> AppContextM ()
removeAssets documentTemplateUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString documentTemplateUuid])

removeAsset :: U.UUID -> U.UUID -> AppContextM ()
removeAsset documentTemplateUuid assetUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

makeBucket :: AppContextM ()
makeBucket = createMakeBucketFn

purgeBucket :: AppContextM ()
purgeBucket = createPurgeBucketFn

removeBucket :: AppContextM ()
removeBucket = createRemoveBucketFn
