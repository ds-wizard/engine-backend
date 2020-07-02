module Wizard.Service.Template.TemplateService where

import Control.Lens ((^.), (^..))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import qualified Registry.Api.Resource.Template.TemplateSimpleDTO as R_TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Template.Template
import Shared.Service.Template.TemplateUtil
import Shared.Util.Identifier
import Shared.Util.List (foldInContext)
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Template.TemplateMapper
import Wizard.Service.Template.TemplateUtil
import Wizard.Service.Template.TemplateValidation

getTemplates :: [(String, String)] -> Maybe String -> AppContextM [Template]
getTemplates queryParams mPkgId = do
  validatePackageIdFormat' mPkgId
  templates <- findTemplatesFiltered queryParams
  return $ filterTemplates mPkgId templates

getTemplatesDto :: [(String, String)] -> Maybe String -> AppContextM [TemplateSimpleDTO]
getTemplatesDto queryParams mPkgId = do
  checkPermission _DMP_PERM
  validatePackageIdFormat' mPkgId
  tmls <- findTemplatesFiltered queryParams
  tmlRs <- retrieveTemplates
  orgRs <- retrieveOrganizations
  pkgs <- findPackages
  let tmls' = filterTemplates mPkgId tmls
  foldInContext . mapToSimpleDTO tmlRs orgRs pkgs . chooseTheNewest . groupTemplates $ tmls'
  where
    mapToSimpleDTO ::
         [R_TemplateSimpleDTO.TemplateSimpleDTO]
      -> [OrganizationSimpleDTO]
      -> [Package]
      -> [Template]
      -> [AppContextM TemplateSimpleDTO]
    mapToSimpleDTO pkgRs orgRs pkgs tmls =
      fmap
        (\tml -> do
           let versions = tmls ^.. traverse . version
           let usablePackages = getUsablePackagesForTemplate tml pkgs
           return $ toSimpleDTO' tml pkgRs orgRs versions usablePackages)
        tmls

getTemplateByUuid :: String -> Maybe String -> AppContextM Template
getTemplateByUuid templateId mPkgId = do
  templates <- getTemplates [] mPkgId
  case L.find (\t -> (t ^. tId) == templateId) templates of
    Just template -> return template
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getTemplateByUuidDto :: String -> AppContextM TemplateDetailDTO
getTemplateByUuidDto templateId = do
  checkPermission _TML_PERM
  tml <- findTemplateById templateId
  pkgs <- findPackages
  versions <- getTemplateVersions tml
  tmlRs <- retrieveTemplates
  orgRs <- retrieveOrganizations
  serverConfig <- asks _appContextServerConfig
  let registryLink = buildTemplateUrl (serverConfig ^. registry . clientUrl) templateId
  let usablePackages = getUsablePackagesForTemplate tml pkgs
  return $ toDetailDTO tml tmlRs orgRs versions registryLink usablePackages

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

deleteTemplatesByQueryParams :: [(String, String)] -> AppContextM ()
deleteTemplatesByQueryParams queryParams = do
  checkPermission _TML_PERM
  tmls <- findTemplatesFiltered queryParams
  traverse_ validateTemplateDeletation (_templateTId <$> tmls)
  deleteTemplatesFiltered queryParams

deleteTemplate :: String -> AppContextM ()
deleteTemplate tmlId = do
  checkPermission _TML_PERM
  tml <- findTemplateById tmlId
  validateTemplateDeletation tmlId
  deleteTemplateById tmlId
  let assetUuids = fmap (\a -> ("filename", U.toString $ a ^. uuid)) (tml ^. assets)
  deleteTemplateAssetContentsFiltered assetUuids

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateVersions :: Template -> AppContextM [String]
getTemplateVersions tml = do
  allTmls <- findTemplatesByOrganizationIdAndKmId (tml ^. organizationId) (tml ^. templateId)
  return . fmap _templateVersion $ allTmls
