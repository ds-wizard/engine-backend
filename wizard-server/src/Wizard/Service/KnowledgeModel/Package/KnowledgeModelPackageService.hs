module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper hiding (toSuggestionDTO)
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Wizard.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService

getPackagesPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page KnowledgeModelPackageSimpleDTO)
getPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort = do
  checkPermission _PM_READ_PERM
  tcRegistry <- getCurrentTenantConfigRegistry
  if mOutdated == Just True && not tcRegistry.enabled
    then return $ Page "knowledgeModelPackages" (PageMetadata 0 0 0 0) []
    else do
      packages <- findPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort
      return . fmap (toSimpleDTO'' tcRegistry.enabled) $ packages

getPackageSuggestions
  :: Maybe String
  -> Maybe [String]
  -> Maybe [String]
  -> Maybe KnowledgeModelPackagePhase
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page KnowledgeModelPackageSuggestion)
getPackageSuggestions mQuery mSelectIds mExcludeIds mPhase mNonEditable pageable sort = do
  checkPermission _PM_READ_PERM
  findPackageSuggestionsPage mQuery mSelectIds mExcludeIds mPhase mNonEditable pageable sort

getPackageById :: String -> AppContextM KnowledgeModelPackage
getPackageById pkgId = resolvePackageId pkgId >>= findPackageById

getPackageDetailById :: String -> Bool -> AppContextM KnowledgeModelPackageDetailDTO
getPackageDetailById pkgId excludeDeprecatedVersions = do
  resolvedPkgId <- resolvePackageId pkgId
  checkIfPackageIsPublic (Just resolvedPkgId) _PM_READ_PERM
  serverConfig <- asks serverConfig
  pkg <- getPackageById resolvedPkgId
  versions <- getPackageVersions pkg excludeDeprecatedVersions
  pkgRs <- findRegistryPackages
  orgRs <- findRegistryOrganizations
  tcRegistry <- getCurrentTenantConfigRegistry
  return $ toDetailDTO pkg tcRegistry.enabled pkgRs orgRs versions (buildPackageUrl serverConfig.registry.clientUrl pkg pkgRs)

getAllPreviousEventsSincePackageId :: String -> AppContextM [KnowledgeModelEvent]
getAllPreviousEventsSincePackageId pkgId = do
  package <- findPackageById pkgId
  packageEvents <- findPackageEvents pkgId
  case package.previousPackageId of
    Just previousPackageId -> do
      pkgEvents <- getAllPreviousEventsSincePackageId previousPackageId
      return $ pkgEvents ++ fmap toEvent packageEvents
    Nothing -> return (fmap toEvent packageEvents)

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM [KnowledgeModelEvent]
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return []
        else do
          package <- findPackageById pkgId
          packageEvents <- findPackageEvents pkgId
          case package.previousPackageId of
            Just previousPackageId -> do
              pkgEvents <- go previousPackageId
              return $ pkgEvents ++ fmap toEvent packageEvents
            Nothing -> return (fmap toEvent packageEvents)

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Maybe KnowledgeModelPackage)
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId = do
  packages <- findPackagesByOrganizationIdAndKmId organizationId kmId
  if null packages
    then return Nothing
    else do
      let sorted = sortPackagesByVersion packages
      return . Just . head $ sorted

getNewerPackages :: String -> Bool -> AppContextM [KnowledgeModelPackage]
getNewerPackages currentPkgId excludeDeprecatedVersions = do
  pkgs <- findPackagesByOrganizationIdAndKmId (getOrgIdFromCoordinate currentPkgId) (getKmIdFromCoordinate currentPkgId)
  return . sortPackagesByVersion . filter (filterPkg excludeDeprecatedVersions) $ pkgs
  where
    filterPkg :: Bool -> KnowledgeModelPackage -> Bool
    filterPkg True pkg = compareVersion pkg.version (getVersionFromCoordinate currentPkgId) == GT && pkg.phase == ReleasedKnowledgeModelPackagePhase
    filterPkg False pkg = compareVersion pkg.version (getVersionFromCoordinate currentPkgId) == GT

createPackage :: (KnowledgeModelPackage, [KnowledgeModelPackageEvent]) -> AppContextM KnowledgeModelPackageSimpleDTO
createPackage (pkg, pkgEvents) =
  runInTransaction $ do
    checkPackageLimit
    insertPackage pkg
    traverse_ insertPackageEvent pkgEvents
    return . toSimpleDTO $ pkg

modifyPackage :: String -> KnowledgeModelPackageChangeDTO -> AppContextM KnowledgeModelPackageChangeDTO
modifyPackage pkgId reqDto =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    _ <- findPackageById pkgId
    updatePackagePhaseById pkgId reqDto.phase
    return reqDto

deletePackagesByQueryParams :: [(String, String)] -> AppContextM ()
deletePackagesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    packages <- findPackagesFiltered queryParams
    let pIds = fmap (.pId) packages
    validatePackagesDeletion pIds
    deletePackagesFiltered queryParams
    return ()

deletePackage :: String -> AppContextM ()
deletePackage pkgId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    package <- findPackageById pkgId
    validatePackageDeletion pkgId
    deletePackageById pkgId
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: KnowledgeModelPackage -> Bool -> AppContextM [String]
getPackageVersions pkg excludeDeprecatedVersions = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . fmap (.version) . filter (filterPkg excludeDeprecatedVersions) $ allPkgs
  where
    filterPkg :: Bool -> KnowledgeModelPackage -> Bool
    filterPkg True pkg = pkg.phase == ReleasedKnowledgeModelPackagePhase
    filterPkg False _ = True
