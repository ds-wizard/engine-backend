module Wizard.Service.PackageBundle.PackageBundleService
  ( exportPackageBundle
  , pullPackageBundleFromRegistry
  , importPackageBundleFromFile
  , importAndConvertPackageBundle
  , importPackageBundle
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find)

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.PackageBundle.PackageBundle
import Wizard.Api.Resource.Package.PackageDTO
import Wizard.Api.Resource.Package.PackageJM ()
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.PackageBundle.PackageBundleDTO
import Wizard.Api.Resource.PackageBundle.PackageBundleJM ()
import Wizard.Constant.Component
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelValidation
import Wizard.Service.Migration.Metamodel.MigratorService
import qualified Wizard.Service.Package.PackageMapper as PM
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation
import Wizard.Service.PackageBundle.PackageBundleMapper
import Wizard.Util.List (foldMaybesInContext)
import Wizard.Util.Logger (logWarnU, msg)

exportPackageBundle :: String -> AppContextM (Either AppError PackageBundleDTO)
exportPackageBundle pbId =
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

pullPackageBundleFromRegistry :: String -> AppContextM (Maybe AppError)
pullPackageBundleFromRegistry pkgId = do
  appConfig <- asks _appContextApplicationConfig
  ePb <- retrievePackageBundleById (appConfig ^. registry) pkgId
  case ePb of
    Right pb -> do
      ePkgs <- importAndConvertPackageBundle pb
      case ePkgs of
        Right _ -> return Nothing
        Left error -> return . Just $ error
    Left error ->
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR 404)
        then return . Just . NotExistsError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId
        else return . Just $ error

importPackageBundleFromFile :: BS.ByteString -> AppContextM (Either AppError [PackageSimpleDTO])
importPackageBundleFromFile = importAndConvertPackageBundle

importAndConvertPackageBundle :: BS.ByteString -> AppContextM (Either AppError [PackageSimpleDTO])
importAndConvertPackageBundle content =
  let eitherDeserializedContent = eitherDecode content
   in case eitherDeserializedContent of
        Right deserializedContent ->
          heMigratePackageBundle deserializedContent $ \encodedPb ->
            case eitherDecode . encode $ encodedPb of
              Right pb -> importPackageBundle pb
              Left error -> do
                logWarnU $
                  msg _CMP_SERVICE ("Couln't deserialize migrated PackageBundle content (" ++ (show error) ++ ")")
                return . Left . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
        Left error -> do
          logWarnU $ msg _CMP_SERVICE ("Couln't deserialize PackageBundle content (" ++ (show error) ++ ")")
          return . Left . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

importPackageBundle :: PackageBundleDTO -> AppContextM (Either AppError [PackageSimpleDTO])
importPackageBundle pb =
  heExtractMainPackage pb $ \pkg ->
    heValidatePackageIdUniqueness (pkg ^. pId) $ do
      let importedPackages = importPackage <$> (pb ^. packages)
      foldMaybesInContext importedPackages
  where
    heExtractMainPackage pb callback =
      case find (\p -> p ^. pId == pb ^. bundleId) (pb ^. packages) of
        Just pkg -> callback pkg
        Nothing -> return . Left . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: PackageDTO -> AppContextM (Either AppError (Maybe PackageSimpleDTO))
importPackage dto = do
  let pkg = PM.fromDTO dto
  skipIfPackageIsAlreadyImported pkg $
    heValidatePackageIdWithCoordinates (pkg ^. pId) (pkg ^. organizationId) (pkg ^. kmId) (pkg ^. version) $
    heValidateMaybePreviousPackageIdExistence (pkg ^. pId) (pkg ^. previousPackageId) $
    heValidateKmValidity (pkg ^. events) (pkg ^. previousPackageId) $ do
      createdPkg <- createPackage pkg
      return . Right . Just $ createdPkg
  where
    skipIfPackageIsAlreadyImported pkg callback = do
      eitherPackage <- findPackageById (pkg ^. pId)
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Right $ Nothing
        Left error -> return . Left $ error
