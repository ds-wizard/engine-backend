module Wizard.Service.Template.TemplateService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Database.DAO.Common
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateGroup
import Shared.Service.Template.TemplateMapper
import Shared.Service.Template.TemplateUtil
import Shared.Util.Coordinate
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Acl.AclService
import Wizard.Service.Coordinate.CoordinateValidation
import Wizard.Service.Template.TemplateMapper
import Wizard.Service.Template.TemplateUtil
import Wizard.Service.Template.TemplateValidation

getTemplates :: [(String, String)] -> Maybe String -> AppContextM [Template]
getTemplates queryParams mPkgId = do
  validateCoordinateFormat' mPkgId
  tmls <- findTemplatesFiltered queryParams
  return $ filterTemplates mPkgId tmls

getTemplatesPage ::
     Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page TemplateSimpleDTO)
getTemplatesPage mOrganizationId mTemplateId mQuery pageable sort = do
  checkPermission _DMP_PERM
  groups <- findTemplateGroups mOrganizationId mTemplateId mQuery pageable sort
  tmlRs <- retrieveTemplates
  orgRs <- retrieveOrganizations
  pkgs <- findPackages
  return . fmap (toSimpleDTO'' tmlRs orgRs pkgs) $ groups

getTemplateSuggestions :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page TemplateSuggestionDTO)
getTemplateSuggestions mPkgId mQuery pageable sort = do
  checkPermission _DMP_PERM
  validateCoordinateFormat' mPkgId
  groups <- findTemplateGroups Nothing Nothing mQuery (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toSuggestionDTO . updatePage . fmap chooseNewest . fmap filterTemplatesInGroup $ groups
  where
    updatePage :: Page (Maybe Template) -> Page Template
    updatePage (Page name _ array) =
      let updatedArray = take updatedSize . catMaybes $ array
          updatedSize = fromMaybe 20 $ pageable ^. size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 $ pageable ^. page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    chooseNewest :: [Template] -> Maybe Template
    chooseNewest [] = Nothing
    chooseNewest [tml] = Just tml
    chooseNewest tmls = Just $ L.maximumBy (\t1 t2 -> compareVersion (t1 ^. version) (t2 ^. version)) tmls
    filterTemplatesInGroup :: TemplateGroup -> [Template]
    filterTemplatesInGroup tmlGroup = filter isTemplateSupported . filterTemplates mPkgId $ (tmlGroup ^. versions)

getTemplatesDto :: [(String, String)] -> Maybe String -> AppContextM [TemplateSimpleDTO]
getTemplatesDto queryParams mPkgId = do
  checkPermission _DMP_PERM
  tmls <- getTemplates queryParams mPkgId
  tmlRs <- retrieveTemplates
  orgRs <- retrieveOrganizations
  pkgs <- findPackages
  return . fmap (toSimpleDTO' tmlRs orgRs pkgs) . chooseTheNewest . groupTemplates $ tmls

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
  validateNewTemplate template
  insertTemplate template
  return template

modifyTemplate :: String -> TemplateChangeDTO -> AppContextM Template
modifyTemplate tmlId reqDto = do
  checkPermission _TML_PERM
  template <- findTemplateById tmlId
  let templateUpdated = fromChangeDTO reqDto template
  validateExistingTemplate templateUpdated
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
