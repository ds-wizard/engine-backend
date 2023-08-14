module Wizard.Service.DocumentTemplate.DocumentTemplateService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (fromMaybe)

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
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Document.DocumentCleanService
import Wizard.Service.DocumentTemplate.DocumentTemplateMapper
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
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
  validateCoordinateFormat' False mPkgId
  templates <- findDocumentTemplatesFiltered queryParams
  return $ filterDocumentTemplates mPkgId templates

getDocumentTemplatesPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Pageable
  -> [Sort]
  -> AppContextM (Page DocumentTemplateSimpleDTO)
getDocumentTemplatesPage mOrganizationId mTemplateId mQuery mTemplateState pageable sort = do
  checkPermission _DOC_TML_READ_PERM
  appConfig <- getAppConfig
  if mTemplateState == (Just . show $ OutdatedDocumentTemplateState) && not appConfig.registry.enabled
    then return $ Page "documentTemplates" (PageMetadata 0 0 0 0) []
    else do
      templates <- findDocumentTemplatesPage mOrganizationId mTemplateId mQuery mTemplateState Nothing pageable sort
      packages <- findPackages
      return . fmap (toSimpleDTO' appConfig.registry.enabled packages) $ templates

getDocumentTemplateSuggestions :: Maybe String -> Bool -> Maybe DocumentTemplatePhase -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateSuggestionDTO)
getDocumentTemplateSuggestions mPkgId includeUnsupportedMetamodelVersion mPhase mQuery mNonEditable pageable sort = do
  checkPermission _DOC_TML_READ_PERM
  validateCoordinateFormat' False mPkgId
  page <- findDocumentTemplatesPage Nothing Nothing mQuery Nothing mNonEditable (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toSuggestionDTO . updatePage page . filterDocumentTemplatesInGroup $ page
  where
    updatePage :: Page DocumentTemplateList -> [DocumentTemplateList] -> Page DocumentTemplateList
    updatePage (Page name _ _) array =
      let updatedArray = take updatedSize array
          updatedSize = fromMaybe 20 pageable.size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 pageable.page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    filterDocumentTemplatesInGroup :: Page DocumentTemplateList -> [DocumentTemplateList]
    filterDocumentTemplatesInGroup page =
      filter (isDocumentTemplateSupported includeUnsupportedMetamodelVersion)
        . filter (isDocumentTemplateInPhase mPhase)
        . filterDocumentTemplates mPkgId
        $ page.entities

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
  return $ toDetailDTO tml tmlRs orgRs versions registryLink usablePackages

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
    traverse_ (\t -> cleanTemporallyDocumentsForTemplate t.tId) tmls
    traverse_ (\t -> validateDocumentTemplateDeletation t.tId) tmls
    deleteDocumentTemplatesFiltered queryParams
    return ()

deleteDocumentTemplate :: String -> AppContextM ()
deleteDocumentTemplate tmlId =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    tml <- findDocumentTemplateById tmlId
    assets <- findAssetsByDocumentTemplateId tmlId
    cleanTemporallyDocumentsForTemplate tmlId
    validateDocumentTemplateDeletation tmlId
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
