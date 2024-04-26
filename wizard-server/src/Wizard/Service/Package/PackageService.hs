module Wizard.Service.Package.PackageService where

import Control.Monad.Reader (asks)

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Api.Resource.Package.PackageChangeDTO
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Package.PackageSuggestion
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Package.PackageUtil
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Service.Package.PackageMapper hiding (toSuggestionDTO)
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

getPackagesPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page PackageSimpleDTO)
getPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort = do
  checkPermission _PM_READ_PERM
  tenantConfig <- getCurrentTenantConfig
  if mOutdated == Just True && not (tenantConfig.registry.enabled)
    then return $ Page "packages" (PageMetadata 0 0 0 0) []
    else do
      packages <- findPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort
      return . fmap (toSimpleDTO'' tenantConfig.registry.enabled) $ packages

getPackageSuggestions
  :: Maybe String
  -> Maybe [String]
  -> Maybe [String]
  -> Maybe PackagePhase
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page PackageSuggestion)
getPackageSuggestions mQuery mSelectIds mExcludeIds mPhase mNonEditable pageable sort = do
  checkPermission _PM_READ_PERM
  findPackageSuggestionsPage mQuery mSelectIds mExcludeIds mPhase mNonEditable pageable sort

getPackageById :: String -> AppContextM Package
getPackageById pkgId = resolvePackageId pkgId >>= findPackageById

getPackageDetailById :: String -> Bool -> AppContextM PackageDetailDTO
getPackageDetailById pkgId excludeDeprecatedVersions = do
  resolvedPkgId <- resolvePackageId pkgId
  checkIfPackageIsPublic (Just resolvedPkgId) _PM_READ_PERM
  serverConfig <- asks serverConfig
  pkg <- getPackageById resolvedPkgId
  versions <- getPackageVersions pkg excludeDeprecatedVersions
  pkgRs <- findRegistryPackages
  orgRs <- findRegistryOrganizations
  tenantConfig <- getCurrentTenantConfig
  return $ toDetailDTO pkg tenantConfig.registry.enabled pkgRs orgRs versions (buildPackageUrl serverConfig.registry.clientUrl pkg pkgRs)

getAllPreviousEventsSincePackageId :: String -> AppContextM [Event]
getAllPreviousEventsSincePackageId pkgId = do
  package <- findPackageWithEventsById pkgId
  case package.previousPackageId of
    Just previousPackageId -> do
      pkgEvents <- getAllPreviousEventsSincePackageId previousPackageId
      return $ pkgEvents ++ package.events
    Nothing -> return package.events

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM [Event]
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return []
        else do
          package <- findPackageWithEventsById pkgId
          case package.previousPackageId of
            Just previousPackageId -> do
              pkgEvents <- go previousPackageId
              return $ pkgEvents ++ package.events
            Nothing -> return package.events

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Maybe Package)
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId = do
  packages <- findPackagesByOrganizationIdAndKmId organizationId kmId
  if null packages
    then return Nothing
    else do
      let sorted = sortPackagesByVersion packages
      return . Just . head $ sorted

getNewerPackages :: String -> Bool -> AppContextM [Package]
getNewerPackages currentPkgId excludeDeprecatedVersions = do
  pkgs <- findPackagesByOrganizationIdAndKmId (getOrgIdFromCoordinate currentPkgId) (getKmIdFromCoordinate currentPkgId)
  return . sortPackagesByVersion . filter (filterPkg excludeDeprecatedVersions) $ pkgs
  where
    filterPkg :: Bool -> Package -> Bool
    filterPkg True pkg = compareVersion pkg.version (getVersionFromCoordinate currentPkgId) == GT && pkg.phase == ReleasedPackagePhase
    filterPkg False pkg = compareVersion pkg.version (getVersionFromCoordinate currentPkgId) == GT

createPackage :: PackageWithEvents -> AppContextM PackageSimpleDTO
createPackage pkg =
  runInTransaction $ do
    checkPackageLimit
    insertPackage pkg
    return . toSimpleDTO . toPackage $ pkg

modifyPackage :: String -> PackageChangeDTO -> AppContextM PackageChangeDTO
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
    validatePackagesDeletation pIds
    deletePackagesFiltered queryParams
    return ()

deletePackage :: String -> AppContextM ()
deletePackage pkgId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    package <- findPackageById pkgId
    validatePackageDeletation pkgId
    deletePackageById pkgId
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> Bool -> AppContextM [String]
getPackageVersions pkg excludeDeprecatedVersions = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . fmap (.version) . filter (filterPkg excludeDeprecatedVersions) $ allPkgs
  where
    filterPkg :: Bool -> Package -> Bool
    filterPkg True pkg = pkg.phase == ReleasedPackagePhase
    filterPkg False _ = True
