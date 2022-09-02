module Wizard.Service.Template.TemplateService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Service.Coordinate.CoordinateValidation
import qualified Shared.Service.Template.TemplateMapper as STM
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.DAO.Template.TemplateDAO (findTemplatesPage)
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Template.TemplateList
import Wizard.S3.Template.TemplateS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Template.TemplateMapper
import Wizard.Service.Template.TemplateUtil
import Wizard.Service.Template.TemplateValidation

getTemplates :: [(String, String)] -> Maybe String -> AppContextM [Template]
getTemplates queryParams mPkgId = do
  validateCoordinateFormat' False mPkgId
  tmls <- findTemplatesFiltered queryParams
  return $ filterTemplates mPkgId tmls

getTemplatesPage ::
     Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page TemplateSimpleDTO)
getTemplatesPage mOrganizationId mTemplateId mQuery pageable sort = do
  checkPermission _DMP_PERM
  tmls <- findTemplatesPage mOrganizationId mTemplateId mQuery pageable sort
  pkgs <- findPackages
  return . fmap (toSimpleDTO' pkgs) $ tmls

getTemplateSuggestions :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page TemplateSuggestionDTO)
getTemplateSuggestions mPkgId mQuery pageable sort = do
  checkPermission _DMP_PERM
  validateCoordinateFormat' False mPkgId
  page <- findTemplatesPage Nothing Nothing mQuery (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toSuggestionDTO . updatePage page . filterTemplatesInGroup $ page
  where
    updatePage :: Page TemplateList -> [TemplateList] -> Page TemplateList
    updatePage (Page name _ _) array =
      let updatedArray = take updatedSize array
          updatedSize = fromMaybe 20 $ pageable ^. size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 $ pageable ^. page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    filterTemplatesInGroup :: Page TemplateList -> [TemplateList]
    filterTemplatesInGroup page = filter isTemplateSupported . filterTemplates mPkgId $ (page ^. entities)

getTemplatesDto :: [(String, String)] -> AppContextM [TemplateSuggestionDTO]
getTemplatesDto queryParams = do
  checkPermission _DMP_PERM
  tmls <- findTemplatesFiltered queryParams
  return . fmap STM.toSuggestionDTO $ tmls

getTemplateByUuidAndPackageId :: String -> Maybe String -> AppContextM Template
getTemplateByUuidAndPackageId templateId mPkgId = do
  templates <- getTemplates [] mPkgId
  case L.find (\t -> (t ^. tId) == templateId) templates of
    Just template -> return template
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getTemplateByUuidDto :: String -> AppContextM TemplateDetailDTO
getTemplateByUuidDto templateId = do
  tml <- findTemplateById templateId
  pkgs <- findPackages
  versions <- getTemplateVersions tml
  tmlRs <- findRegistryTemplates
  orgRs <- findRegistryOrganizations
  serverConfig <- asks _appContextServerConfig
  let registryLink = buildTemplateUrl (serverConfig ^. registry . clientUrl) templateId
  let usablePackages = getUsablePackagesForTemplate tml pkgs
  return $ toDetailDTO tml tmlRs orgRs versions registryLink usablePackages

createTemplate :: TemplateChangeDTO -> AppContextM Template
createTemplate reqDto =
  runInTransaction $ do
    checkPermission _TML_PERM
    checkTemplateLimit
    now <- liftIO getCurrentTime
    appUuid <- asks _appContextAppUuid
    let template = fromCreateDTO reqDto appUuid now
    validateNewTemplate template
    insertTemplate template
    return template

modifyTemplate :: String -> TemplateChangeDTO -> AppContextM Template
modifyTemplate tmlId reqDto =
  runInTransaction $ do
    checkPermission _TML_PERM
    template <- findTemplateById tmlId
    let templateUpdated = fromChangeDTO reqDto template
    validateExistingTemplate templateUpdated
    updateTemplateById templateUpdated
    deleteTemporalDocumentsByTemplateId tmlId
    return templateUpdated

deleteTemplatesByQueryParams :: [(String, String)] -> AppContextM ()
deleteTemplatesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _TML_PERM
    tmls <- findTemplatesFiltered queryParams
    traverse_ validateTemplateDeletation (_templateTId <$> tmls)
    deleteTemplatesFiltered queryParams
    return ()

deleteTemplate :: String -> AppContextM ()
deleteTemplate tmlId =
  runInTransaction $ do
    checkPermission _TML_PERM
    tml <- findTemplateById tmlId
    assets <- findTemplateAssetsByTemplateId tmlId
    validateTemplateDeletation tmlId
    deleteTemplateById tmlId
    let assetUuids = fmap (\a -> U.toString $ a ^. uuid) assets
    traverse_ (removeAsset tmlId) assetUuids

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateVersions :: Template -> AppContextM [String]
getTemplateVersions tml = do
  allTmls <- findTemplatesByOrganizationIdAndKmId (tml ^. organizationId) (tml ^. templateId)
  return . fmap _templateVersion $ allTmls
