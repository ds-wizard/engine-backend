module Wizard.Service.Template.Asset.TemplateAssetService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Util.Uuid
import Wizard.Database.DAO.Template.TemplateDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL

getTemplateAssets :: String -> AppContextM [TemplateAsset]
getTemplateAssets tmlId = do
  checkPermission _TML_PERM
  tml <- findTemplateById tmlId
  return $ tml ^. assets

getTemplateAsset :: String -> String -> AppContextM TemplateAsset
getTemplateAsset tmlId assetUuid = do
  checkPermission _TML_PERM
  tml <- findTemplateById tmlId
  case L.find (\a -> U.toString (a ^. uuid) == assetUuid) (tml ^. assets) of
    Just asset -> return asset
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "asset" assetUuid

getTemplateAssetContent :: String -> String -> AppContextM (TemplateAsset, BS.ByteString)
getTemplateAssetContent tmlId assetUuid = do
  asset <- getTemplateAsset tmlId assetUuid
  content <- findTemplateAssetContent (U.toString $ asset ^. uuid)
  return (asset, content)

createAsset :: String -> String -> String -> BS.ByteString -> AppContextM TemplateAsset
createAsset tmlId fileName contentType content
  -- 1. Check permission
 = do
  checkPermission _TML_PERM
  -- 2. Check existence of template
  tml <- findTemplateById tmlId
  -- 3. Create asset
  aUuid <- liftIO generateUuid
  let newAsset =
        TemplateAsset
          {_templateAssetUuid = aUuid, _templateAssetFileName = fileName, _templateAssetContentType = contentType}
  insertTemplateAssetContent (U.toString aUuid) content
  -- 4. Add to template
  let updatedTml = tml & assets .~ ((tml ^. assets) ++ [newAsset])
  updateTemplateById updatedTml
  -- 5. Return asset
  return newAsset

deleteTemplateAsset :: String -> String -> AppContextM ()
deleteTemplateAsset tmlId assetUuid = do
  checkPermission _TML_PERM
  asset <- getTemplateAsset tmlId assetUuid
  tml <- findTemplateById tmlId
  let updatedTml = tml & assets .~ filter (\a -> a ^. uuid /= asset ^. uuid) (tml ^. assets)
  updateTemplateById updatedTml
  deleteTemplateAssetContentsFiltered [("filename", assetUuid)]
