module Registry.Service.PackageBundle.PackageBundleService (
  getPackageBundle,
  importPackageBundle,
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
import Registry.Service.Package.PackageService
import Registry.Service.PackageBundle.PackageBundleAcl
import Registry.Service.PackageBundle.PackageBundleMapper
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.Package.PackageJM ()
import qualified Shared.Api.Resource.PackageBundle.PackageBundleDTO as S_PackageBundleDTO
import Shared.Constant.KnowledgeModel
import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Package.PackageWithEvents
import Shared.Model.Package.PackageWithEventsRaw
import qualified Shared.Service.Package.PackageMapper as PM
import Shared.Service.Package.PackageUtil

getPackageBundle :: String -> AppContextM R_PackageBundleDTO.PackageBundleDTO
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

importPackageBundle :: S_PackageBundleDTO.PackageBundleDTO -> AppContextM S_PackageBundleDTO.PackageBundleDTO
importPackageBundle pb =
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
