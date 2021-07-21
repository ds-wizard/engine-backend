module Wizard.Service.TemplateBundle.TemplateBundleService where

import Control.Lens ((^.))
import Control.Monad.Except (catchError, throwError)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import LensesConfig
import Shared.Api.Resource.TemplateBundle.TemplateBundleDTO
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Service.Template.TemplateMapper
import Shared.Service.TemplateBundle.TemplateBundleMapper
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.S3.Template.TemplateS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Template.TemplateValidation

exportTemplateBundle :: String -> AppContextM BSL.ByteString
exportTemplateBundle tmlId =
  runInTransaction $ do
    template <- findTemplateById tmlId
    files <- findTemplateFilesByTemplateId tmlId
    assets <- findTemplateAssetsByTemplateId tmlId
    assetContents <- traverse (findAsset (template ^. tId)) assets
    return $ toTemplateArchive (toTemplateBundle template files assets) assetContents

pullTemplateBundleFromRegistry :: String -> AppContextM ()
pullTemplateBundleFromRegistry tmlId =
  runInTransaction $ do
    checkPermission _TML_PERM
    tb <- catchError (retrieveTemplateBundleById tmlId) handleError
    _ <- importAndConvertTemplateBundle tb
    return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_TB__PULL_NON_EXISTING_TML tmlId
        else throwError error

importAndConvertTemplateBundle :: BSL.ByteString -> AppContextM TemplateBundleDTO
importAndConvertTemplateBundle contentS =
  case fromTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      let template = fromTemplateBundle bundle
      validateNewTemplate template
      deleteOldTemplateIfPresent bundle
      traverse_ (\(a, content) -> putAsset (template ^. tId) (U.toString $ a ^. uuid) content) assetContents
      insertTemplate template
      traverse_ (insertTemplateFile . fromFileDTO (template ^. tId)) (bundle ^. files)
      traverse_ (insertTemplateAsset . fromAssetDTO (template ^. tId)) (bundle ^. assets)
      return bundle
    Left error -> throwError error

deleteOldTemplateIfPresent :: TemplateBundleDTO -> AppContextM ()
deleteOldTemplateIfPresent bundle =
  runInTransaction $ do
    mOldTemplate <- findTemplateById' (bundle ^. tId)
    mOldAssets <- findTemplateAssetsByTemplateId (bundle ^. tId)
    case mOldTemplate of
      Just oldTemplate -> do
        traverse_ (\a -> removeAsset (oldTemplate ^. tId) (U.toString $ a ^. uuid)) mOldAssets
        deleteTemplateById (oldTemplate ^. tId)
        return ()
      Nothing -> return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: String -> TemplateAsset -> AppContextM (TemplateAsset, BS.ByteString)
findAsset tmlId asset =
  runInTransaction $ do
    content <- getAsset tmlId (U.toString $ asset ^. uuid)
    return (asset, content)
