module Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageService (
  getSimplePackagesFiltered,
  getPackageById,
) where

import qualified Data.List as L

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Registry.Database.DAO.Common
import Registry.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Context.AppContext
import Registry.Service.Audit.AuditService
import Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Common.Util.List (foldInContext)
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO hiding (findPackagesFiltered)
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil

getSimplePackagesFiltered :: [(String, String)] -> Maybe Int -> [(String, String)] -> AppContextM [KnowledgeModelPackageSimpleDTO]
getSimplePackagesFiltered queryParams mMetamodelVersion headers =
  runInTransaction $ do
    _ <- auditListPackages headers
    pkgs <- findPackagesFiltered queryParams mMetamodelVersion
    foldInContext . mapToSimpleDTO . chooseTheNewest . groupPackages $ pkgs
  where
    mapToSimpleDTO :: [KnowledgeModelPackage] -> [AppContextM KnowledgeModelPackageSimpleDTO]
    mapToSimpleDTO =
      fmap
        ( \pkg -> do
            org <- findOrganizationByOrgId pkg.organizationId
            return $ toSimpleDTO pkg org
        )

getPackageById :: String -> AppContextM KnowledgeModelPackageDetailDTO
getPackageById pkgId = do
  resolvedPkgId <- resolvePackageId pkgId
  pkg <- findPackageById resolvedPkgId
  versions <- getPackageVersions pkg
  org <- findOrganizationByOrgId pkg.organizationId
  return $ toDetailDTO pkg versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: KnowledgeModelPackage -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . L.sort . fmap (.version) $ allPkgs
