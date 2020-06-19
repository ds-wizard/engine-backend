module Wizard.Service.TemplateBundle.TemplateBundleService where

import Control.Lens ((^.))
import Control.Monad.Except (catchError, throwError)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Wizard.Database.DAO.Template.TemplateDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL
import Wizard.Service.TemplateBundle.TemplateBundleMapper

pullTemplateBundleFromRegistry :: String -> AppContextM ()
pullTemplateBundleFromRegistry tmlId = do
  checkPermission _TML_PERM
  tb <- catchError (retrieveTemplateBundleById tmlId) handleError
  _ <- importAndConvertTemplateBundle tb
  return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR 404)
        then throwError . NotExistsError $ _ERROR_SERVICE_TB__PULL_NON_EXISTING_TML tmlId
        else throwError error

importAndConvertTemplateBundle :: BSL.ByteString -> AppContextM Template
importAndConvertTemplateBundle contentS =
  case fromTemplateArchive contentS of
    Right (template, assets) -> do
      deleteOldTemplateIfPresent template
      traverse_ (\(a, content) -> insertTemplateAssetContent (a ^. fileName) content) assets
      insertTemplate template
      return template
    Left error -> throwError error

deleteOldTemplateIfPresent :: Template -> AppContextM ()
deleteOldTemplateIfPresent template = do
  mOldTemplate <- findTemplateById' (template ^. tId)
  case mOldTemplate of
    Just oldTemplate -> do
      traverse_
        (\a -> deleteTemplateAssetContentsFiltered [("filename", U.toString $ a ^. uuid)])
        (oldTemplate ^. assets)
      deleteTemplateById (oldTemplate ^. tId)
    Nothing -> return ()
