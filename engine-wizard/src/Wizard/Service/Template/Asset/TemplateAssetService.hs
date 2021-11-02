module Wizard.Service.Template.Asset.TemplateAssetService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Model.Template.Template
import Shared.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.Template.TemplateS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Template.Asset.TemplateAssetMapper
import Wizard.Service.Template.TemplateValidation

getTemplateAssets :: String -> AppContextM [TemplateAsset]
getTemplateAssets tmlId =
  runInTransaction $ do
    checkPermission _TML_PERM
    findTemplateAssetsByTemplateId tmlId

getTemplateAsset :: String -> AppContextM TemplateAsset
getTemplateAsset assetUuid =
  runInTransaction $ do
    checkPermission _TML_PERM
    findTemplateAssetById assetUuid

getTemplateAssetContent :: String -> String -> AppContextM (TemplateAsset, BS.ByteString)
getTemplateAssetContent tmlId assetUuid =
  runInTransaction $ do
    asset <- findTemplateAssetById assetUuid
    content <- getAsset tmlId (U.toString $ asset ^. uuid)
    return (asset, content)

createAsset :: String -> String -> String -> BS.ByteString -> AppContextM TemplateAsset
createAsset tmlId fileName contentType content =
  runInTransaction $ do
    checkPermission _TML_PERM
    validateTemplateFileAndAssetUniqueness Nothing tmlId fileName
    aUuid <- liftIO generateUuid
    appUuid <- asks _appContextAppUuid
    let newAsset = fromChangeDTO tmlId aUuid fileName contentType appUuid
    insertTemplateAsset newAsset
    putAsset tmlId (U.toString aUuid) content
    deleteTemporalDocumentsByTemplateAssetId (U.toString aUuid)
    return newAsset

deleteTemplateAsset :: String -> String -> AppContextM ()
deleteTemplateAsset tmlId assetUuid =
  runInTransaction $ do
    checkPermission _TML_PERM
    asset <- findTemplateAssetById assetUuid
    deleteTemplateAssetById (U.toString $ asset ^. uuid)
    removeAsset tmlId assetUuid
    deleteTemporalDocumentsByTemplateAssetId assetUuid
    return ()
