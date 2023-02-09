module Registry.S3.DocumentTemplate.DocumentTemplateS3 where

import qualified Data.ByteString.Char8 as BS

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "templates"

retrieveAsset :: String -> String -> AppContextM BS.ByteString
retrieveAsset templateId assetId = createGetObjectFn (f' "%s/%s/%s" [folderName, templateId, assetId])

putAsset :: String -> String -> String -> BS.ByteString -> AppContextM String
putAsset templateId assetId contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, templateId, assetId]) (Just contentType) Nothing

removeAssets :: String -> AppContextM ()
removeAssets templateId = createRemoveObjectFn (f' "%s/%s" [folderName, templateId])

removeAsset :: String -> String -> AppContextM ()
removeAsset templateId assetId = createRemoveObjectFn (f' "%s/%s/%s" [folderName, templateId, assetId])

makeBucket :: AppContextM ()
makeBucket = createMakeBucketFn

purgeBucket :: AppContextM ()
purgeBucket = createPurgeBucketFn

removeBucket :: AppContextM ()
removeBucket = createRemoveBucketFn
