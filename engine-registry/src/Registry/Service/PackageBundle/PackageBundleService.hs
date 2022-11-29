module Registry.Service.PackageBundle.PackageBundleService (
  getPackageBundle,
) where

import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()
import Registry.Model.Context.AppContext
import Registry.Model.PackageBundle.PackageBundle
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageService
import Registry.Service.PackageBundle.PackageBundleMapper
import Shared.Constant.KnowledgeModel
import Shared.Model.Package.PackageWithEventsRaw
import Shared.Service.Package.PackageUtil

getPackageBundle :: String -> AppContextM PackageBundleDTO
getPackageBundle pbId = do
  _ <- auditGetPackageBundle pbId
  resolvedPbId <- resolvePackageId pbId
  packages <- getSeriesOfPackages resolvedPbId
  let newestPackage = last packages
  let pb =
        PackageBundle
          { bundleId = newestPackage.pId
          , name = newestPackage.name
          , organizationId = newestPackage.organizationId
          , kmId = newestPackage.kmId
          , version = newestPackage.version
          , metamodelVersion = kmMetamodelVersion
          , packages = packages
          }
  return . toDTO $ pb
