module Wizard.S3.Template.TemplateS3 where

import qualified Data.ByteString.Char8 as BS

import Shared.S3.Common
import Shared.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "templates"

getAsset :: String -> String -> AppContextM BS.ByteString
getAsset templateId assetId = createGetObjectFn (f' "%s/%s/%s" [folderName, templateId, assetId])

putAsset :: String -> String -> BS.ByteString -> AppContextM String
putAsset templateId assetId = createPutObjectFn (f' "%s/%s/%s" [folderName, templateId, assetId]) Nothing

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
