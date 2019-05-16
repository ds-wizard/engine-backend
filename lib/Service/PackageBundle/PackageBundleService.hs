module Service.PackageBundle.PackageBundleService
  ( exportPackageBundle
  , importPackageBundleFromFile
  , importPackageBundle
  ) where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find)

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageWithEventsDTO
import Api.Resource.PackageBundle.PackageBundleDTO
import Api.Resource.PackageBundle.PackageBundleJM ()
import Constant.KnowledgeModel
import Database.DAO.Package.PackageDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.PackageBundle.PackageBundle
import qualified Service.Event.EventMapper as EM
import Service.KnowledgeModel.KnowledgeModelValidation
import Service.Migration.Metamodel.MigratorService
import Service.Package.PackageService
import Service.Package.PackageValidation
import Service.PackageBundle.PackageBundleMapper
import Util.List (foldMaybesInContext)

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

importPackageBundleFromFile :: BS.ByteString -> AppContextM (Either AppError [PackageDTO])
importPackageBundleFromFile fileContent =
  let eitherDeserializedFile = eitherDecode fileContent
  in case eitherDeserializedFile of
       Right deserializedFile ->
         heMigratePackageBundle deserializedFile $ \encodedPb ->
           case eitherDecode . encode $ encodedPb of
             Right pb -> importPackageBundle pb
             Left error -> return . Left . createErrorWithErrorMessage $ error
       Left error -> return . Left . createErrorWithErrorMessage $ error

importPackageBundle :: PackageBundleDTO -> AppContextM (Either AppError [PackageDTO])
importPackageBundle pb =
  heExtractMainPackage pb $ \pkg ->
    heValidatePackageIdUniqueness (pkg ^. pId) $ do
      let importedPackages = importPackage <$> (pb ^. packages)
      foldMaybesInContext importedPackages
  where
    heExtractMainPackage pb callback =
      case find (\p -> p ^. pId == pb ^. bundleId) (pb ^. packages) of
        Just pkg -> callback pkg
        Nothing -> return . Left . createErrorWithErrorMessage $ _ERROR_SERVICE_PB__MAIN_PKG_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: PackageWithEventsDTO -> AppContextM (Either AppError (Maybe PackageDTO))
importPackage pkg = do
  let ppId = pkg ^. pId
  let pName = pkg ^. name
  let pOrganizationId = pkg ^. organizationId
  let pKmId = pkg ^. kmId
  let pVersion = pkg ^. version
  let pDescription = pkg ^. description
  let pParentPackageId = pkg ^. parentPackageId
  let pEvents = EM.fromDTOFn <$> (pkg ^. events)
  skipIfPackageIsAlreadyImported pkg $
    heValidatePackageIdWithCoordinates ppId pOrganizationId pKmId pVersion $
    heValidateMaybeParentPackageIdExistence ppId pParentPackageId $
    heValidateKmValidity pEvents pParentPackageId $ do
      createdPkg <- createPackage pName pOrganizationId pKmId pVersion pDescription pParentPackageId pEvents
      return . Right . Just $ createdPkg
  where
    skipIfPackageIsAlreadyImported pkg callback = do
      eitherPackage <- findPackageById (pkg ^. pId)
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Right $ Nothing
        Left error -> return . Left $ error
