module Registry.Service.Package.PackageService (
  getSimplePackagesFiltered,
  getPackageById,
  getSeriesOfPackages,
) where

import qualified Data.List as L

import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Database.DAO.Common
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.DAO.Package.PackageDAO
import Registry.Model.Context.AppContext
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageMapper
import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import Shared.Common.Util.List (foldInContext)
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO hiding (findPackagesFiltered)
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEventsRaw
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

getSimplePackagesFiltered :: [(String, String)] -> Maybe Int -> [(String, String)] -> AppContextM [PackageSimpleDTO]
getSimplePackagesFiltered queryParams mMetamodelVersion headers =
  runInTransaction $ do
    _ <- auditListPackages headers
    pkgs <- findPackagesFiltered queryParams mMetamodelVersion
    foldInContext . mapToSimpleDTO . chooseTheNewest . groupPackages $ pkgs
  where
    mapToSimpleDTO :: [Package] -> [AppContextM PackageSimpleDTO]
    mapToSimpleDTO =
      fmap
        ( \pkg -> do
            org <- findOrganizationByOrgId pkg.organizationId
            return $ toSimpleDTO pkg org
        )

getPackageById :: String -> AppContextM PackageDetailDTO
getPackageById pkgId = do
  resolvedPkgId <- resolvePackageId pkgId
  pkg <- findPackageById resolvedPkgId
  versions <- getPackageVersions pkg
  org <- findOrganizationByOrgId pkg.organizationId
  return $ toDetailDTO pkg versions org

getSeriesOfPackages :: String -> AppContextM [PackageWithEventsRaw]
getSeriesOfPackages pkgId = do
  package <- findPackageWithEventsRawById pkgId
  case package.previousPackageId of
    Just parentPkgId -> do
      parentPackages <- getSeriesOfPackages parentPkgId
      return $ parentPackages ++ [package]
    Nothing -> return [package]

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . L.sort . fmap (.version) $ allPkgs
