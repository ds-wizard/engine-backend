module Registry.Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackageById
  , getSeriesOfPackages
  ) where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Database.DAO.Organization.OrganizationSqlDAO
import Registry.Model.Context.AppContext
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageMapper
import Shared.Database.DAO.CommonSql
import Shared.Database.DAO.Package.PackageSqlDAO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Shared.Util.List (foldInContext)

getSimplePackagesFiltered :: [(String, String)] -> [(String, String)] -> AppContextM [PackageSimpleDTO]
getSimplePackagesFiltered queryParams headers =
  runInTransaction $ do
    _ <- auditListPackages headers
    pkgs <- findPackagesFiltered queryParams
    foldInContext . mapToSimpleDTO . chooseTheNewest . groupPackages $ pkgs
  where
    mapToSimpleDTO :: [Package] -> [AppContextM PackageSimpleDTO]
    mapToSimpleDTO =
      fmap
        (\pkg -> do
           org <- findOrganizationByOrgId (pkg ^. organizationId)
           return $ toSimpleDTO pkg org)

getPackageById :: String -> AppContextM PackageDetailDTO
getPackageById pkgId =
  runInTransaction $ do
    pkg <- findPackageById pkgId
    versions <- getPackageVersions pkg
    org <- findOrganizationByOrgId (pkg ^. organizationId)
    return $ toDetailDTO pkg versions org

getSeriesOfPackages :: String -> AppContextM [PackageWithEvents]
getSeriesOfPackages pkgId =
  runInTransaction $ do
    package <- findPackageWithEventsById pkgId
    case package ^. previousPackageId of
      Just parentPkgId -> do
        parentPackages <- getSeriesOfPackages parentPkgId
        return $ parentPackages ++ [package]
      Nothing -> return [package]

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId)
  return . L.sort . fmap _packageVersion $ allPkgs
