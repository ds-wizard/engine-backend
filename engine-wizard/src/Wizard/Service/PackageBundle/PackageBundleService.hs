module Wizard.Service.PackageBundle.PackageBundleService
  ( exportPackageBundle
  , pullPackageBundleFromRegistry
  , importPackageBundleFromFile
  , importAndConvertPackageBundle
  , importPackageBundle
  ) where

import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find)
import Data.Maybe (catMaybes)

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.Package.PackageJM ()
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Api.Resource.PackageBundle.PackageBundleJM ()
import Shared.Constant.Component
import Shared.Constant.KnowledgeModel
import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.PackageBundle.PackageBundle
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.PackageBundle.PackageBundleMapper
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Acl.AclService
import Wizard.Service.KnowledgeModel.KnowledgeModelValidation
import Wizard.Service.Migration.Metamodel.MigratorService
import qualified Wizard.Service.Package.PackageMapper as PM
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation
import Wizard.Util.Logger

exportPackageBundle :: String -> AppContextM PackageBundleDTO
exportPackageBundle pbId =
  runInTransaction $ do
    packages <- getSeriesOfPackages pbId
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

pullPackageBundleFromRegistry :: String -> AppContextM ()
pullPackageBundleFromRegistry pkgId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    pb <- catchError (retrievePackageBundleById pkgId) handleError
    _ <- importAndConvertPackageBundle pb
    return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId
        else throwError error

importPackageBundleFromFile :: BS.ByteString -> AppContextM [PackageSimpleDTO]
importPackageBundleFromFile = importAndConvertPackageBundle

importAndConvertPackageBundle :: BS.ByteString -> AppContextM [PackageSimpleDTO]
importAndConvertPackageBundle contentS =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    case eitherDecode contentS of
      Right content -> do
        encodedPb <- migratePackageBundle content
        case eitherDecode . encode $ encodedPb of
          Right pb -> importPackageBundle pb
          Left error -> do
            logWarnU _CMP_SERVICE ("Couln't deserialize migrated PackageBundle content (" ++ show error ++ ")")
            throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
      Left error -> do
        logWarnU _CMP_SERVICE ("Couln't deserialize PackageBundle content (" ++ show error ++ ")")
        throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

importPackageBundle :: PackageBundleDTO -> AppContextM [PackageSimpleDTO]
importPackageBundle pb =
  runInTransaction $ do
    pkg <- extractMainPackage pb
    validatePackageIdUniqueness (pkg ^. pId)
    pkgs <- forM (pb ^. packages) importPackage
    return . catMaybes $ pkgs
  where
    extractMainPackage pb =
      case find (\p -> p ^. pId == pb ^. bundleId) (pb ^. packages) of
        Just pkg -> return pkg
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: PackageDTO -> AppContextM (Maybe PackageSimpleDTO)
importPackage dto =
  runInTransaction $ do
    appUuid <- asks _appContextAppUuid
    let pkg = PM.fromDTO dto appUuid
    skipIfPackageIsAlreadyImported pkg $ do
      validateCoordinateWithParams (pkg ^. pId) (pkg ^. organizationId) (pkg ^. kmId) (pkg ^. version)
      validateMaybePreviousPackageIdExistence (pkg ^. pId) (pkg ^. previousPackageId)
      validateKmValidity (pkg ^. events) (pkg ^. previousPackageId)
      createdPkg <- createPackage pkg
      return . Just $ createdPkg
  where
    skipIfPackageIsAlreadyImported pkg callback = do
      eitherPackage <- findPackageById' (pkg ^. pId)
      case eitherPackage of
        Nothing -> callback
        Just _ -> return Nothing
