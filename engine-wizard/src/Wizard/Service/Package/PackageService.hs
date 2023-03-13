module Wizard.Service.Package.PackageService where

import Control.Monad.Reader (asks)

import Shared.Database.DAO.Package.PackageDAO
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageMapper hiding (toSuggestionDTO)
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Api.Resource.Package.PackageChangeDTO
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Package.PackageState
import Wizard.Model.Package.PackageSuggestion
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Package.PackageUtil
import Wizard.Service.Package.PackageValidation

getPackagesPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Pageable
  -> [Sort]
  -> AppContextM (Page PackageSimpleDTO)
getPackagesPage mOrganizationId mKmId mQuery mPackageState pageable sort = do
  checkPermission _PM_READ_PERM
  appConfig <- getAppConfig
  if mPackageState == (Just . show $ OutdatedPackageState) && not (appConfig.registry.enabled)
    then return $ Page "packages" (PageMetadata 0 0 0 0) []
    else do
      packages <- findPackagesPage mOrganizationId mKmId mQuery mPackageState pageable sort
      return . fmap (toSimpleDTO'' (appConfig.registry.enabled)) $ packages

getPackageSuggestions
  :: Maybe String
  -> Maybe [String]
  -> Maybe [String]
  -> Maybe PackagePhase
  -> Pageable
  -> [Sort]
  -> AppContextM (Page PackageSuggestion)
getPackageSuggestions mQuery mSelectIds mExcludeIds mPhase pageable sort = do
  checkPermission _PM_READ_PERM
  findPackageSuggestionsPage mQuery mSelectIds mExcludeIds mPhase pageable sort

getPackageById :: String -> AppContextM Package
getPackageById pkgId = resolvePackageId pkgId >>= findPackageById

getPackageDetailById :: String -> AppContextM PackageDetailDTO
getPackageDetailById pkgId = do
  resolvedPkgId <- resolvePackageId pkgId
  checkIfPackageIsPublic (Just resolvedPkgId) _PM_READ_PERM
  serverConfig <- asks serverConfig
  pkg <- getPackageById resolvedPkgId
  versions <- getPackageVersions pkg
  pkgRs <- findRegistryPackages
  orgRs <- findRegistryOrganizations
  return $ toDetailDTO pkg pkgRs orgRs versions (buildPackageUrl serverConfig.registry.clientUrl pkg pkgRs)

getSeriesOfPackages :: String -> AppContextM [PackageWithEvents]
getSeriesOfPackages pkgId = do
  pkg <- findPackageWithEventsById pkgId
  case pkg.previousPackageId of
    Just previousPkgId -> do
      previousPkgs <- getSeriesOfPackages previousPkgId
      return $ previousPkgs ++ [pkg]
    Nothing -> return [pkg]

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

getNewerPackages :: String -> AppContextM [Package]
getNewerPackages currentPkgId = do
  pkgs <- findPackagesByOrganizationIdAndKmId (getOrgIdFromCoordinate currentPkgId) (getKmIdFromCoordinate currentPkgId)
  let newerPkgs = filter (\pkg -> compareVersion pkg.version (getVersionFromCoordinate currentPkgId) == GT) pkgs
  return . sortPackagesByVersion $ newerPkgs

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
getPackageVersions :: Package -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . fmap (.version) $ allPkgs
