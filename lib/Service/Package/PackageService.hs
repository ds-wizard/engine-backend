module Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackageById
  , getSeriesOfPackages
  , getAllPreviousEventsSincePackageId
  , getAllPreviousEventsSincePackageIdAndUntilPackageId
  , getTheNewestPackageByOrganizationIdAndKmId
  , getNewerPackages
  , createPackage
  , deletePackagesByQueryParams
  , deletePackage
  -- Helpers
  , heGetSeriesOfPackages
  , heGetAllPreviousEventsSincePackageId
  , hmGetAllPreviousEventsSincePackageId
  , heGetAllPreviousEventsSincePackageIdAndUntilPackageId
  , heGetNewerPackages
  ) where

import Control.Lens ((^.), (^..), traverse)
import Control.Monad.Reader (asks)
import Data.List (maximumBy)
import Data.Maybe
import Data.Text (Text)

import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import Database.DAO.Package.PackageDAO
import Integration.Http.Registry.Runner
import Integration.Resource.Package.PackageSimpleIDTO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Event.Event
import Model.Package.Package
import Model.Package.PackageWithEvents
import Service.Package.PackageMapper
import Service.Package.PackageUtils
import Service.Package.PackageValidation
import Service.Statistics.StatisticsService
import Util.List (groupBy)

getSimplePackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams = do
  dswConfig <- asks _appContextAppConfig
  heFindPackagesFiltered queryParams $ \pkgs ->
    heGetInstanceStatistics $ \iStat ->
      heRetrievePackages (dswConfig ^. registry) iStat $ \pkgRs ->
        return . Right . fmap (toSimpleDTOs pkgRs) . groupPkgs $ pkgs
  where
    groupPkgs :: [Package] -> [[Package]]
    groupPkgs = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))
    toSimpleDTOs :: [PackageSimpleIDTO] -> [Package] -> PackageSimpleDTO
    toSimpleDTOs pkgRs pkgs = toSimpleDTO' newestPkg pkgRs (pkgs ^.. traverse . version)
      where
        newestPkg = maximumBy (\p1 p2 -> compare (p1 ^. version) (p2 ^. version)) pkgs

getPackageById :: String -> AppContextM (Either AppError PackageDetailDTO)
getPackageById pkgId = do
  dswConfig <- asks _appContextAppConfig
  heFindPackageById pkgId $ \pkg ->
    heGetPackageVersions pkg $ \versions ->
      heGetInstanceStatistics $ \iStat ->
        heRetrievePackages (dswConfig ^. registry) iStat $ \pkgRs ->
          return . Right $ toDetailDTO pkg pkgRs versions (buildPackageUrl (dswConfig ^. registry . clientUrl) pkgId)

getSeriesOfPackages :: String -> AppContextM (Either AppError [PackageWithEvents])
getSeriesOfPackages pkgId =
  heFindPackageWithEventsById pkgId $ \pkg ->
    case pkg ^. previousPackageId of
      Just previousPkgId ->
        heGetSeriesOfPackages previousPkgId $ \previousPkgs -> return . Right $ previousPkgs ++ [pkg]
      Nothing -> return . Right $ [pkg]

getAllPreviousEventsSincePackageId :: String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageId pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. previousPackageId of
      Just previousPackageId ->
        heGetAllPreviousEventsSincePackageId previousPackageId $ \pkgEvents ->
          return . Right $ pkgEvents ++ (package ^. events)
      Nothing -> return . Right $ package ^. events

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return . Right $ []
        else heFindPackageWithEventsById pkgId $ \package ->
               case package ^. previousPackageId of
                 Just previousPackageId -> do
                   eitherPkgEvents <- go previousPackageId
                   case eitherPkgEvents of
                     Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
                     Left error -> return . Left $ error
                 Nothing -> return . Right $ package ^. events

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError (Maybe Package))
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId =
  heFindPackagesByOrganizationIdAndKmId organizationId kmId $ \packages -> do
    if length packages == 0
      then return . Right $ Nothing
      else do
        let sorted = sortPackagesByVersion packages
        return . Right . Just . head $ sorted

getNewerPackages :: String -> AppContextM (Either AppError [Package])
getNewerPackages currentPkgId =
  heFindPackagesByOrganizationIdAndKmId (getOrgIdFromPkgId currentPkgId) (getKmIdFromPkgId currentPkgId) $ \pkgs -> do
    let newerPkgs = filter (\pkg -> compareVersion (pkg ^. version) (getVersionFromPkgId currentPkgId) == GT) pkgs
    return . Right . sortPackagesByVersion $ newerPkgs

createPackage :: PackageWithEvents -> AppContextM PackageSimpleDTO
createPackage pkg = do
  insertPackage pkg
  return . toSimpleDTO . toPackage $ pkg

deletePackagesByQueryParams :: [(Text, Text)] -> AppContextM (Maybe AppError)
deletePackagesByQueryParams queryParams =
  hmFindPackagesFiltered queryParams $ \packages -> do
    maybeError <- validatePackagesDeletation (_packagePId <$> packages)
    if isJust maybeError
      then return maybeError
      else do
        deletePackagesFiltered queryParams
        return Nothing

deletePackage :: String -> AppContextM (Maybe AppError)
deletePackage pkgId =
  hmFindPackageById pkgId $ \package -> do
    maybeError <- validatePackageDeletation pkgId
    if isJust maybeError
      then return maybeError
      else do
        deletePackageById pkgId
        return Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM (Either AppError [String])
getPackageVersions pkg =
  heFindPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId) $ \allPkgs ->
    return . Right . fmap _packageVersion $ allPkgs

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetSeriesOfPackages pkgId callback = do
  eitherPackages <- getSeriesOfPackages pkgId
  case eitherPackages of
    Right package -> callback package
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetAllPreviousEventsSincePackageId pkgId callback = do
  eitherEvents <- getAllPreviousEventsSincePackageId pkgId
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Left $ error

hmGetAllPreviousEventsSincePackageId pkgId callback = do
  eitherEvents <- getAllPreviousEventsSincePackageId pkgId
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Just $ error

-- -----------------------------------------------------
heGetAllPreviousEventsSincePackageIdAndUntilPackageId since until callback = do
  eitherEvents <- getAllPreviousEventsSincePackageIdAndUntilPackageId since until
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetNewerPackages currentPkgId callback = do
  eitherPackages <- getNewerPackages currentPkgId
  case eitherPackages of
    Right packages -> callback packages
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetPackageVersions pkg callback = do
  eitherResult <- getPackageVersions pkg
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
