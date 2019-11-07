module Service.PackageBundle.PackageBundleService
  ( getPackageBundle
  ) where

import Control.Lens ((^.))

import Api.Resource.PackageBundle.PackageBundleDTO
import Api.Resource.PackageBundle.PackageBundleJM ()
import Constant.KnowledgeModel
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.PackageBundle.PackageBundle
import Service.Audit.AuditService
import Service.Package.PackageService
import Service.PackageBundle.PackageBundleMapper

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
