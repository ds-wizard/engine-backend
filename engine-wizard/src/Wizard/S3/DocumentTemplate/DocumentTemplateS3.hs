module Wizard.S3.DocumentTemplate.DocumentTemplateS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.S3.Common
import Shared.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "templates"

retrieveAsset :: String -> U.UUID -> AppContextM BS.ByteString
retrieveAsset documentTemplateId assetUuid = createGetObjectFn (f' "%s/%s/%s" [folderName, documentTemplateId, U.toString assetUuid])

putAsset :: String -> U.UUID -> String -> BS.ByteString -> AppContextM String
putAsset documentTemplateId assetUuid contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, documentTemplateId, U.toString assetUuid]) (Just contentType)

makePublicLink :: String -> U.UUID -> AppContextM String
makePublicLink documentTemplateId assetUuid = createMakePublicLink folderName (f' "%s/%s" [documentTemplateId, U.toString assetUuid])

removeAssets :: String -> AppContextM ()
removeAssets documentTemplateId = createRemoveObjectFn (f' "%s/%s" [folderName, documentTemplateId])

removeAsset :: String -> U.UUID -> AppContextM ()
removeAsset documentTemplateId assetUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, documentTemplateId, U.toString assetUuid])

makeBucket :: AppContextM ()
makeBucket = createMakeBucketFn

purgeBucket :: AppContextM ()
purgeBucket = createPurgeBucketFn

removeBucket :: AppContextM ()
removeBucket = createRemoveBucketFn
