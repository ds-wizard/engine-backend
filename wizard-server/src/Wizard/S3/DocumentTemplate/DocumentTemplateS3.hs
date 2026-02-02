module Wizard.S3.DocumentTemplate.DocumentTemplateS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "document-templates"

retrieveAsset :: U.UUID -> U.UUID -> AppContextM BS.ByteString
retrieveAsset documentTemplateUuid assetUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

putAsset :: U.UUID -> U.UUID -> String -> BS.ByteString -> AppContextM String
putAsset documentTemplateUuid assetUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid]) (Just contentType) Nothing

presignGetAssetUrl :: U.UUID -> U.UUID -> Int -> AppContextM String
presignGetAssetUrl documentTemplateUuid assetUuid = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString documentTemplateUuid, U.toString assetUuid])

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
