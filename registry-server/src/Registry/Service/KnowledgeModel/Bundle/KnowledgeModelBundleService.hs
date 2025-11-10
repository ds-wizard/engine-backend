module Registry.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService (
  exportBundle,
  importBundle,
) where

import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.UUID as U

import Registry.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Registry.Database.DAO.Common
import Registry.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRaw ()
import Registry.Model.Context.AppContext
import Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import qualified Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle as R_KnowledgeModelBundle
import Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Registry.Service.Audit.AuditService
import Registry.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAcl
import Shared.Common.Constant.Tenant
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Localization.Messages.Public
import qualified Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle as S_KnowledgeModelBundle
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PM
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil

exportBundle :: String -> AppContextM R_KnowledgeModelBundle.KnowledgeModelBundle
exportBundle pbId = do
  _ <- auditGetKnowledgeModelBundle pbId
  resolvedPbId <- resolvePackageId pbId
  packages <- findSeriesOfPackagesRecursiveById resolvedPbId
  case lastSafe packages of
    Just newestPackage -> do
      let pb =
            R_KnowledgeModelBundle.KnowledgeModelBundle
              { bundleId = newestPackage.pId
              , name = newestPackage.name
              , organizationId = newestPackage.organizationId
              , kmId = newestPackage.kmId
              , version = newestPackage.version
              , metamodelVersion = kmMetamodelVersion
              , packages = packages
              }
      return pb
    Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "knowledge_model_package" [("tenant_uuid", U.toString defaultTenantUuid), ("id", resolvedPbId)]

importBundle :: S_KnowledgeModelBundle.KnowledgeModelBundle -> AppContextM S_KnowledgeModelBundle.KnowledgeModelBundle
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
importPackage :: KnowledgeModelBundlePackage -> AppContextM ()
importPackage dto =
  runInTransaction $ do
    let (pkg, pkgEvents) = PM.fromKnowledgeModelBundlePackage dto U.nil
    eitherPackage <- findPackageById' pkg.pId
    case eitherPackage of
      Nothing -> do
        insertPackage pkg
        traverse_ insertPackageEvent pkgEvents
      Just _ -> return ()
