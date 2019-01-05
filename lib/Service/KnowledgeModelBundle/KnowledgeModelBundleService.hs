module Service.KnowledgeModelBundle.KnowledgeModelBundleService
  ( exportKnowledgeModelBundle
  , importKnowledgeModelBundleFromFile
  , importKnowledgeModelBundle
  ) where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find)

import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleDTO
import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleJM ()
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageWithEventsDTO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.KnowledgeModelBundle.KnowledgeModelBundle
import qualified Service.Event.EventMapper as EM
import Service.KnowledgeModel.KnowledgeModelValidation
import Service.KnowledgeModelBundle.KnowledgeModelBundleMapper
import Service.Package.PackageService
import Service.Package.PackageValidation
import Util.List (foldMaybesInContext)

exportKnowledgeModelBundle :: String -> AppContextM (Either AppError KnowledgeModelBundleDTO)
exportKnowledgeModelBundle kmbId =
  heGetSeriesOfPackages kmbId $ \packages -> do
    let newestPackage = last packages
    let kmb =
          KnowledgeModelBundle
          { _knowledgeModelBundleBundleId = newestPackage ^. pId
          , _knowledgeModelBundleName = newestPackage ^. name
          , _knowledgeModelBundleOrganizationId = newestPackage ^. organizationId
          , _knowledgeModelBundleKmId = newestPackage ^. kmId
          , _knowledgeModelBundleVersion = newestPackage ^. version
          , _knowledgeModelBundlePackages = packages
          }
    return . Right . toDTO $ kmb

importKnowledgeModelBundleFromFile :: BS.ByteString -> AppContextM (Either AppError [PackageDTO])
importKnowledgeModelBundleFromFile fileContent =
  let eitherDeserializedFile = eitherDecode fileContent
  in case eitherDeserializedFile of
       Right deserializedFile -> importKnowledgeModelBundle deserializedFile
       Left error -> return . Left . createErrorWithErrorMessage $ error

importKnowledgeModelBundle :: KnowledgeModelBundleDTO -> AppContextM (Either AppError [PackageDTO])
importKnowledgeModelBundle kmb =
  heExtractMainPackage kmb $ \pkg ->
    heValidatePackageIdUniqueness (pkg ^. pId) $ do
      let importedPackages = importPackage <$> (kmb ^. packages)
      foldMaybesInContext importedPackages
  where
    heExtractMainPackage kmb callback =
      case find (\p -> p ^. pId == kmb ^. bundleId) (kmb ^. packages) of
        Just pkg -> callback pkg
        Nothing -> return . Left . createErrorWithErrorMessage $ _ERROR_SERVICE_KMB__MAIN_PKG_ABSENCE

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
