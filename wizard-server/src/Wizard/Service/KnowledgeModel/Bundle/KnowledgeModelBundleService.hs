module Wizard.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService (
  getTemporaryFileWithBundle,
  exportBundle,
  pullBundleFromRegistry,
  importAndConvertBundle,
  importBundle,
) where

import Control.Monad (forM, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Component
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Localization.Messages.Public
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import qualified Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as KnowledgeModelPackageMapper
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage ()
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAudit
import Wizard.Service.KnowledgeModel.KnowledgeModelValidation
import Wizard.Service.KnowledgeModel.Metamodel.MigrationService
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation (
  validateMaybePreviousPackageIdExistence,
  validatePackageIdUniqueness,
 )
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithBundle :: U.UUID -> AppContextM TemporaryFileDTO
getTemporaryFileWithBundle uuid =
  runInTransaction $ do
    bundle <- exportBundle uuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.km" [show . createCoordinate $ bundle]) "application/octet-stream" mCurrentUserUuid (encode bundle)
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportBundle :: U.UUID -> AppContextM KnowledgeModelBundle
exportBundle uuid =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    packages <- findSeriesOfPackagesRecursiveByUuid uuid
    case lastSafe packages of
      Just newestPackage -> do
        when
          newestPackage.nonEditable
          (throwError . UserError $ _ERROR_SERVICE_PKG__NON_EDITABLE_PKG)
        let bundle =
              KnowledgeModelBundle
                { bundleId = newestPackage.pId
                , name = newestPackage.name
                , organizationId = newestPackage.organizationId
                , kmId = newestPackage.kmId
                , version = newestPackage.version
                , metamodelVersion = knowledgeModelMetamodelVersion
                , packages = packages
                }
        auditKnowledgeModelBundleExport (show bundle.bundleId)
        return bundle
      Nothing -> throwError . UserError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG (show uuid)

pullBundleFromRegistry :: String -> AppContextM KnowledgeModelPackageSimpleDTO
pullBundleFromRegistry pkgId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    checkPackageLimit
    pb <- catchError (retrieveKnowledgeModelBundleById pkgId) handleError
    importAndConvertBundle pb True
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId
        else throwError error

importAndConvertBundle :: BSL.ByteString -> Bool -> AppContextM KnowledgeModelPackageSimpleDTO
importAndConvertBundle contentS fromRegistry =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    checkPackageLimit
    case eitherDecode contentS of
      Right content -> do
        encodedPb <- migrateKnowledgeModelBundle content
        case eitherDecode . encode $ encodedPb of
          Right pb -> do
            if fromRegistry
              then auditKnowledgeModelBundlePullFromRegistry (show pb.bundleId)
              else auditKnowledgeModelBundleImportFromFile (show pb.bundleId)
            importBundle pb
          Left error -> do
            logWarnI _CMP_SERVICE ("Could not deserialize migrated Knowledge Model Bundle content (" ++ show error ++ ")")
            throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
      Left error -> do
        logWarnI _CMP_SERVICE ("Could not deserialize Knowledge Model Bundle content (" ++ show error ++ ")")
        throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

importBundle :: KnowledgeModelBundle -> AppContextM KnowledgeModelPackageSimpleDTO
importBundle pb =
  runInTransaction $ do
    pkg <- extractMainPackage pb
    validatePackageIdUniqueness (createCoordinate pkg)
    pkgs <- forM pb.packages importPackage
    return . last $ catMaybes pkgs
  where
    extractMainPackage pb =
      case find (\p -> p.pId == pb.bundleId) pb.packages of
        Just pkg -> return pkg
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: KnowledgeModelBundlePackage -> AppContextM (Maybe KnowledgeModelPackageSimpleDTO)
importPackage dto =
  runInTransaction $ do
    tenantUuid <- asks currentTenantUuid
    uuid <- liftIO generateUuid
    validateMaybePreviousPackageIdExistence (createCoordinate dto) dto.previousPackageId
    previousPackage <- traverse findPackageByCoordinate dto.previousPackageId
    let (pkg, kmEvents) = KnowledgeModelPackageMapper.fromKnowledgeModelBundlePackage dto uuid (fmap (.uuid) previousPackage) tenantUuid
    skipIfPackageIsAlreadyImported pkg $ do
      let events = fmap KnowledgeModelPackageMapper.toEvent kmEvents
      validateKmValidity events pkg.previousPackageUuid
      let fixedKmEvents = fixTimestampsIfNeeded kmEvents
      createdPkg <- createPackage (pkg, fixedKmEvents)
      return . Just $ createdPkg
  where
    skipIfPackageIsAlreadyImported pkg callback = do
      eitherPackage <- findPackageByCoordinate' (createCoordinate pkg)
      case eitherPackage of
        Nothing -> callback
        Just _ -> return Nothing

fixTimestampsIfNeeded :: [KnowledgeModelPackageEvent] -> [KnowledgeModelPackageEvent]
fixTimestampsIfNeeded [] = []
fixTimestampsIfNeeded events@(first : _) =
  if isSortedByCreatedAt events
    then events
    else zipWith setNewTime events [0 ..]
  where
    baseTime = first.createdAt
    setNewTime e i = e {createdAt = addUTCTime (fromIntegral i) baseTime} :: KnowledgeModelPackageEvent

isSortedByCreatedAt :: [KnowledgeModelPackageEvent] -> Bool
isSortedByCreatedAt [] = True
isSortedByCreatedAt [_] = True
isSortedByCreatedAt (x : y : rest) = x.createdAt < y.createdAt && isSortedByCreatedAt (y : rest)
