module Registry.Service.PackageBundle.PackageBundleService
  ( getPackageBundle
  ) where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()
import Registry.Model.Context.AppContext
import Registry.Model.PackageBundle.PackageBundle
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageService
import Registry.Service.PackageBundle.PackageBundleMapper
import Shared.Constant.KnowledgeModel
import Shared.Service.Package.PackageUtil

getPackageBundle :: String -> AppContextM PackageBundleDTO
getPackageBundle pbId = do
  _ <- auditGetPackageBundle pbId
  resolvedPbId <- resolvePackageId pbId
  packages <- getSeriesOfPackages resolvedPbId
  let newestPackage = last packages
  let pb =
        PackageBundle
          { _packageBundleBundleId = newestPackage ^. pId
          , _packageBundleName = newestPackage ^. name
          , _packageBundleOrganizationId = newestPackage ^. organizationId
          , _packageBundleKmId = newestPackage ^. kmId
          , _packageBundleVersion = newestPackage ^. version
          , _packageBundleMetamodelVersion = kmMetamodelVersion
          , _packageBundlePackages = packages
          }
  return . toDTO $ pb
