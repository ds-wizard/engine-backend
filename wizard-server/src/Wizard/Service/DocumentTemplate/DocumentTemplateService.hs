module Wizard.Service.DocumentTemplate.DocumentTemplateService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.Document.DocumentCleanService
import Wizard.Service.DocumentTemplate.DocumentTemplateMapper
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO hiding (findDocumentTemplatesFiltered)
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as STM
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO

getDocumentTemplates :: [(String, String)] -> Maybe String -> AppContextM [DocumentTemplate]
getDocumentTemplates queryParams mPkgId = do
  validateCoordinateFormat' False "templateId" mPkgId
  templates <- findDocumentTemplatesFiltered queryParams
  return $ filterDocumentTemplates mPkgId templates

getDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateSimpleDTO)
getDocumentTemplatesPage mOrganizationId mTemplateId mQuery mOutdated pageable sort = do
  checkPermission _DOC_TML_READ_PERM
  tcRegistry <- getCurrentTenantConfigRegistry
  if mOutdated == Just True && not tcRegistry.enabled
    then return $ Page "documentTemplates" (PageMetadata 0 0 0 0) []
    else do
      templates <- findDocumentTemplatesPage mOrganizationId mTemplateId mQuery mOutdated Nothing pageable sort
      return . fmap (toSimpleDTO' tcRegistry.enabled) $ templates

getDocumentTemplateSuggestions :: Maybe String -> Bool -> Maybe DocumentTemplatePhase -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateSuggestionDTO)
getDocumentTemplateSuggestions mPkgId includeUnsupportedMetamodelVersion mPhase mQuery mNonEditable pageable sort = do
  checkPermission _DOC_TML_READ_PERM
  validateCoordinateFormat' False "templateId" mPkgId
  tmls <- findDocumentTemplatesSuggestions mQuery mNonEditable
  let entities = filterDocumentTemplatesInGroup tmls
  return $ toSuggestionDTOPage entities pageable
  where
    filterDocumentTemplatesInGroup :: [DocumentTemplateSuggestion] -> [DocumentTemplateSuggestion]
    filterDocumentTemplatesInGroup =
      filter (isDocumentTemplateSupported includeUnsupportedMetamodelVersion)
        . filter (isDocumentTemplateInPhase mPhase)
        . filterDocumentTemplates mPkgId

getDocumentTemplatesDto :: [(String, String)] -> AppContextM [DocumentTemplateSuggestionDTO]
getDocumentTemplatesDto queryParams = do
  checkPermission _DOC_TML_READ_PERM
  tmls <- findDocumentTemplatesFiltered queryParams
  return . fmap STM.toSuggestionDTO $ tmls

getDocumentTemplateByUuidAndPackageId :: String -> Maybe String -> AppContextM DocumentTemplate
getDocumentTemplateByUuidAndPackageId documentTemplateId mPkgId = do
  templates <- getDocumentTemplates [] mPkgId
  case L.find (\t -> t.tId == documentTemplateId) templates of
    Just tml -> return tml
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getDocumentTemplateByUuidDto :: String -> AppContextM DocumentTemplateDetailDTO
getDocumentTemplateByUuidDto documentTemplateId = do
  resolvedTmlId <- resolveDocumentTemplateId documentTemplateId
  tml <- findDocumentTemplateById resolvedTmlId
  pkgs <- findPackages
  versions <- getDocumentTemplateVersions tml
  tmlRs <- findRegistryTemplates
  orgRs <- findRegistryOrganizations
  serverConfig <- asks serverConfig
  let registryLink = buildRegistryTemplateUrl serverConfig.registry.clientUrl tml tmlRs
  let usablePackages = getUsablePackagesForDocumentTemplate tml pkgs
  tcRegistry <- getCurrentTenantConfigRegistry
  return $ toDetailDTO tml tcRegistry.enabled tmlRs orgRs versions registryLink usablePackages

modifyDocumentTemplate :: String -> DocumentTemplateChangeDTO -> AppContextM DocumentTemplateDetailDTO
modifyDocumentTemplate documentTemplateId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    validateChangeDto documentTemplateId reqDto
    tml <- findDocumentTemplateById documentTemplateId
    let templateUpdated = fromChangeDTO reqDto tml
    validateExistingDocumentTemplate templateUpdated
    updateDocumentTemplateById templateUpdated
    deleteTemporalDocumentsByDocumentTemplateId documentTemplateId
    getDocumentTemplateByUuidDto documentTemplateId

deleteDocumentTemplatesByQueryParams :: [(String, String)] -> AppContextM ()
deleteDocumentTemplatesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    tmls <- findDocumentTemplatesFiltered queryParams
    traverse_ (\t -> deleteDocumentTemplate t.tId) tmls

deleteDocumentTemplate :: String -> AppContextM ()
deleteDocumentTemplate tmlId =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    tml <- findDocumentTemplateById tmlId
    assets <- findAssetsByDocumentTemplateId tmlId
    validateDocumentTemplateDeletation tmlId
    cleanTemporallyDocumentsForTemplate tmlId
    deleteDocumentTemplateById tmlId
    let assetUuids = fmap (.uuid) assets
    traverse_ (removeAsset tmlId) assetUuids

-- --------------------------------
-- PRIVATE
-- --------------------------------
getDocumentTemplateVersions :: DocumentTemplate -> AppContextM [String]
getDocumentTemplateVersions tml = do
  allTmls <- findDocumentTemplatesByOrganizationIdAndKmId tml.organizationId tml.templateId
  return . fmap (.version) . filter (\t -> t.phase == ReleasedDocumentTemplatePhase || t.phase == DeprecatedDocumentTemplatePhase) $ allTmls
