module Registry.Service.PackageBundle.PackageBundleService
  ( getPackageBundle
  ) where

import Control.Lens ((^.))

import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()
import Registry.LensesConfig
import Registry.Model.Context.AppContext
import Registry.Service.Audit.AuditService
import Registry.Service.Package.PackageService
import Registry.Service.PackageBundle.PackageBundleMapper
import Shared.Constant.KnowledgeModel
import Shared.Model.Error.Error
import Shared.Model.PackageBundle.PackageBundle

getPackageBundle :: String -> AppContextM (Either AppError PackageBundleDTO)
getPackageBundle pbId =
  heAuditGetPackageBundle pbId $ \_ ->
    heGetSeriesOfPackages pbId $ \packages -> do
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
      return . Right . toDTO $ pb
