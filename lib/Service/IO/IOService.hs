module Service.IO.IOService
  ( importPackageInFile
  , importPackage
  ) where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageWithEventsDTO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Package.PackageMapper
import Service.Package.PackageService

importPackageInFile :: BS.ByteString -> AppContextM (Either AppError PackageDTO)
importPackageInFile fileContent =
  let eitherDeserializedFile = eitherDecode fileContent
  in case eitherDeserializedFile of
       Right deserializedFile -> importPackage deserializedFile
       Left error -> return . Left . createErrorWithErrorMessage $ error

importPackage :: PackageWithEventsDTO -> AppContextM (Either AppError PackageDTO)
importPackage reqDto = do
  let packageWithEvents = fromDTOWithEvents reqDto
  let pName = packageWithEvents ^. name
  let pOrganizationId = packageWithEvents ^. organizationId
  let pKmId = packageWithEvents ^. kmId
  let pVersion = packageWithEvents ^. version
  let pDescription = packageWithEvents ^. description
  let pParentPackageId = packageWithEvents ^. parentPackageId
  let pEvents = packageWithEvents ^. events
  let pId = buildPackageId pOrganizationId pKmId pVersion
  validatePackageId pId $
    validateParentPackageId pId pParentPackageId $
    validateKmValidity pId pParentPackageId pEvents $ do
      createdPkg <- createPackage pName pOrganizationId pKmId pVersion pDescription pParentPackageId pEvents
      return . Right $ createdPkg
  where
    validatePackageId pkgId callback = do
      eitherPackage <- findPackageById pkgId
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Left . createErrorWithErrorMessage $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId
        Left error -> return . Left $ error
    validateParentPackageId pkgId maybeParentPkgId callback =
      case maybeParentPkgId of
        Just parentPkgId -> do
          eitherPackage <- findPackageById parentPkgId
          case eitherPackage of
            Right _ -> callback
            Left (NotExistsError _) ->
              return . Left . createErrorWithErrorMessage $
              _ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId
            Left error -> return . Left $ error
        Nothing -> callback
    validateKmValidity pkgId maybeParentPkgId pkgEvents callback =
      case maybeParentPkgId of
        Just ppId ->
          heGetAllPreviousEventsSincePackageId ppId $ \eventsFromPackage ->
            heCompileKnowledgeModelFromScratch (eventsFromPackage ++ pkgEvents) $ \_ -> callback
        Nothing -> heCompileKnowledgeModelFromScratch pkgEvents $ \_ -> callback
