module Wizard.Service.Package.Bundle.PackageBundleService (
  getTemporaryFileWithBundle,
  exportBundle,
  pullBundleFromRegistry,
  importAndConvertBundle,
  importBundle,
) where

import Control.Monad (forM, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (find)
import Data.Maybe (catMaybes)

import Shared.Common.Constant.Component
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelValidation
import Wizard.Service.Migration.Metamodel.MigratorService
import Wizard.Service.Package.Bundle.PackageBundleAudit
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation (
  validateMaybePreviousPackageIdExistence,
  validatePackageIdUniqueness,
 )
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageJM ()
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleJM ()

import Wizard.Model.Context.AppContextHelpers
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Localization.Messages.Public
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Model.PackageBundle.PackageBundle
import WizardLib.KnowledgeModel.Service.Package.Bundle.PackageBundleMapper
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PM
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithBundle :: String -> AppContextM TemporaryFileDTO
getTemporaryFileWithBundle pbId =
  runInTransaction $ do
    bundle <- exportBundle pbId
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.km" [pbId]) "application/octet-stream" mCurrentUserUuid (encode bundle)
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportBundle :: String -> AppContextM PackageBundleDTO
exportBundle pbId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    packages <- findSeriesOfPackagesRecursiveById pbId
    let newestPackage = last packages
    when
      newestPackage.nonEditable
      (throwError . UserError $ _ERROR_SERVICE_PKG__NON_EDITABLE_PKG)
    let bundle =
          PackageBundle
            { bundleId = newestPackage.pId
            , name = newestPackage.name
            , organizationId = newestPackage.organizationId
            , kmId = newestPackage.kmId
            , version = newestPackage.version
            , metamodelVersion = kmMetamodelVersion
            , packages = packages
            }
    auditPackageBundleExport pbId
    return . toDTO $ bundle

pullBundleFromRegistry :: String -> AppContextM ()
pullBundleFromRegistry pkgId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    checkPackageLimit
    pb <- catchError (retrievePackageBundleById pkgId) handleError
    _ <- importAndConvertBundle pb True
    return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId
        else throwError error

importAndConvertBundle :: BSL.ByteString -> Bool -> AppContextM [PackageSimpleDTO]
importAndConvertBundle contentS fromRegistry =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    checkPackageLimit
    case eitherDecode contentS of
      Right content -> do
        encodedPb <- migratePackageBundle content
        case eitherDecode . encode $ encodedPb of
          Right pb -> do
            if fromRegistry
              then auditPackageBundlePullFromRegistry pb.bundleId
              else auditPackageBundleImportFromFile pb.bundleId
            importBundle pb
          Left error -> do
            logWarnI _CMP_SERVICE ("Couln't deserialize migrated PackageBundle content (" ++ show error ++ ")")
            throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
      Left error -> do
        logWarnI _CMP_SERVICE ("Couln't deserialize PackageBundle content (" ++ show error ++ ")")
        throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

importBundle :: PackageBundleDTO -> AppContextM [PackageSimpleDTO]
importBundle pb =
  runInTransaction $ do
    pkg <- extractMainPackage pb
    validatePackageIdUniqueness pkg.pId
    pkgs <- forM pb.packages importPackage
    return . catMaybes $ pkgs
  where
    extractMainPackage pb =
      case find (\p -> p.pId == pb.bundleId) pb.packages of
        Just pkg -> return pkg
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: PackageDTO -> AppContextM (Maybe PackageSimpleDTO)
importPackage dto =
  runInTransaction $ do
    tenantUuid <- asks currentTenantUuid
    let pkg = PM.fromDTO dto tenantUuid
    skipIfPackageIsAlreadyImported pkg $ do
      validateCoordinateWithParams pkg.pId pkg.organizationId pkg.kmId pkg.version
      validateMaybePreviousPackageIdExistence pkg.pId pkg.previousPackageId
      validateKmValidity pkg.events pkg.previousPackageId
      createdPkg <- createPackage pkg
      return . Just $ createdPkg
  where
    skipIfPackageIsAlreadyImported pkg callback = do
      eitherPackage <- findPackageById' pkg.pId
      case eitherPackage of
        Nothing -> callback
        Just _ -> return Nothing
