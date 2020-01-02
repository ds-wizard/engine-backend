module Registry.Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackageById
  , getSeriesOfPackages
  -- Helpers
  , heGetSeriesOfPackages
  ) where

import Control.Lens ((^.))
import Data.List (maximumBy)
import Data.Text (Text)

import LensesConfig
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.DAO.Package.PackageDAO
import Registry.Model.Context.AppContext
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageMapper
import Registry.Util.List (foldEithersInContext, groupBy)
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Helper (createHeeHelper)

getSimplePackagesFiltered :: [(Text, Text)] -> [(String, String)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams headers = do
  heAuditListPackages headers $ \_ ->
    heFindPackagesFiltered queryParams $ \pkgs ->
      foldEithersInContext . mapToSimpleDTO . chooseTheNewest . groupPkgs $ pkgs
  where
    groupPkgs :: [Package] -> [[Package]]
    groupPkgs = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))
    chooseTheNewest :: [[Package]] -> [Package]
    chooseTheNewest = fmap (maximumBy (\p1 p2 -> compare (p1 ^. version) (p2 ^. version)))
    mapToSimpleDTO :: [Package] -> [AppContextM (Either AppError PackageSimpleDTO)]
    mapToSimpleDTO =
      fmap (\pkg -> heFindOrganizationByOrgId (pkg ^. organizationId) $ \org -> return . Right $ toSimpleDTO pkg org)

getPackageById :: String -> AppContextM (Either AppError PackageDetailDTO)
getPackageById pkgId =
  heFindPackageById pkgId $ \pkg ->
    heGetPackageVersions pkg $ \versions ->
      heFindOrganizationByOrgId (pkg ^. organizationId) $ \org -> return . Right $ toDetailDTO pkg versions org

getSeriesOfPackages :: String -> AppContextM (Either AppError [PackageWithEvents])
getSeriesOfPackages pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. previousPackageId of
      Just parentPkgId ->
        heGetSeriesOfPackages parentPkgId $ \parentPackages -> return . Right $ parentPackages ++ [package]
      Nothing -> return . Right $ [package]

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
heGetSeriesOfPackages pkgId callback = createHeeHelper (getSeriesOfPackages pkgId) callback

heGetPackageVersions pkg callback = createHeeHelper (getPackageVersions pkg) callback
