module Wizard.Service.Template.TemplateService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.List (find)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Template.TemplateDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Template.TemplateMapper
import Wizard.Service.Template.TemplateUtil
import Wizard.Util.IdentifierUtil

getTemplates :: Maybe String -> AppContextM [Template]
getTemplates mPkgId = do
  templates <- findTemplates
  case mPkgId of
    Nothing -> return templates
    Just pkgId -> do
      validatePackageIdFormat pkgId
      let pkgIdSplit = splitPackageId pkgId
      return . filterTemplates pkgIdSplit $ templates

getTemplatesDto :: Maybe String -> AppContextM [TemplateDTO]
getTemplatesDto mPkgId = do
  checkPermission _DMP_PERM
  templates <- getTemplates mPkgId
  pkgs <- findPackages
  return . fmap (\tml -> toDTO (getAllowedPackagesForTemplate tml pkgs) tml) $ templates

getTemplateByUuid :: String -> Maybe String -> AppContextM Template
getTemplateByUuid templateId mPkgId = do
  templates <- getTemplates mPkgId
  case find (\t -> (t ^. tId) == templateId) templates of
    Just template -> return template
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getTemplateByUuidDto :: String -> AppContextM TemplateSimpleDTO
getTemplateByUuidDto templateId = do
  checkPermission _TML_PERM
  template <- findTemplateById templateId
  return . toSimpleDTO $ template

createTemplate :: TemplateChangeDTO -> AppContextM Template
createTemplate reqDto = do
  checkPermission _TML_PERM
  now <- liftIO getCurrentTime
  let template = fromCreateDTO reqDto now
  insertTemplate template
  return template

modifyTemplate :: String -> TemplateChangeDTO -> AppContextM Template
modifyTemplate tmlId reqDto = do
  checkPermission _TML_PERM
  template <- findTemplateById tmlId
  let templateUpdated = fromChangeDTO reqDto template
  updateTemplateById templateUpdated
  return templateUpdated

deleteTemplate :: String -> AppContextM ()
deleteTemplate tmlId = do
  checkPermission _TML_PERM
  tml <- findTemplateById tmlId
  deleteTemplateById tmlId
  let assetUuids = fmap (\a -> ("filename", U.toString $ a ^. uuid)) (tml ^. assets)
  deleteTemplateAssetContentsFiltered assetUuids
