module Registry.Service.Package.Bundle.PackageBundleService (
  exportBundle,
  importBundle,
) where

import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.UUID as U

import qualified Registry.Api.Resource.PackageBundle.PackageBundleDTO as R_PackageBundleDTO
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()
import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.PackageBundle.PackageBundle
import Registry.Service.Audit.AuditService
import Registry.Service.Package.Bundle.PackageBundleAcl
import Registry.Service.Package.Bundle.PackageBundleMapper
import Shared.Common.Constant.Tenant
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageJM ()
import qualified WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO as S_PackageBundleDTO
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Localization.Messages.Public
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Model.Package.PackageWithEventsRaw
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PM
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

exportBundle :: String -> AppContextM R_PackageBundleDTO.PackageBundleDTO
exportBundle pbId = do
  _ <- auditGetPackageBundle pbId
  resolvedPbId <- resolvePackageId pbId
  packages <- findSeriesOfPackagesRecursiveById resolvedPbId
  case lastSafe packages of
    Just newestPackage -> do
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
    Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "package" [("tenant_uuid", U.toString defaultTenantUuid), ("id", resolvedPbId)]

importBundle :: S_PackageBundleDTO.PackageBundleDTO -> AppContextM S_PackageBundleDTO.PackageBundleDTO
importBundle pb =
  runInTransaction $ do
    checkWritePermission
    pkg <- extractMainPackage pb
    traverse_ importPackage pb.packages
    return pb
  where
    extractMainPackage pb =
      case L.find (\p -> p.pId == pb.bundleId) pb.packages of
        Just pkg -> return pkg
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: PackageDTO -> AppContextM ()
importPackage dto =
  runInTransaction $ do
    let pkg = PM.fromDTO dto U.nil
    eitherPackage <- findPackageById' pkg.pId
    case eitherPackage of
      Nothing -> do
        insertPackage pkg
        return ()
      Just _ -> return ()
