module Service.Package.PackageValidation where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID as U
import Text.Regex

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageWithEventsDTO
import Api.Resource.Version.VersionDTO
import Common.Error
import Common.Localization
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Model.Context.AppContext
import Model.Event.Event
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Organization.OrganizationService
import Service.Package.PackageMapper
import Service.Package.PackageUtils

validatePackagesDeletation :: [String] -> AppContextM (Maybe AppError)
validatePackagesDeletation pkgPIdsToDelete = foldl foldOne (return Nothing) (validateOnePackage <$> pkgPIdsToDelete)
  where
    foldOne :: AppContextM (Maybe AppError) -> AppContextM (Maybe AppError) -> AppContextM (Maybe AppError)
    foldOne accIO resultIO = do
      acc <- accIO
      if isJust acc
        then accIO
        else resultIO
    validateOnePackage :: String -> AppContextM (Maybe AppError)
    validateOnePackage pkgId = do
      eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId pkgId
      case eitherBranches of
        Right [] -> do
          eitherPkgs <- findPackagesByParentPackageId pkgId
          case eitherPkgs of
            Right [] -> return Nothing
            Right pkgs -> do
              if length (filter (filFun) pkgs) > 0
                then return . Just . createErrorWithErrorMessage $
                     _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "package"
                else return Nothing
            Left error -> return . Just $ error
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "branch"
        Left error -> return . Just $ error
    filFun :: Package -> Bool
    filFun p = not ((p ^. pId) `elem` pkgPIdsToDelete)

validatePackageDeletation :: String -> AppContextM (Maybe AppError)
validatePackageDeletation pkgId = do
  eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId pkgId
  case eitherBranches of
    Right [] -> do
      eitherPkgs <- findPackagesByParentPackageId pkgId
      case eitherPkgs of
        Right [] -> return Nothing
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "package"
        Left error -> return . Just $ error
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "branch"
    Left error -> return . Just $ error

validateVersionFormat :: String -> Maybe AppError
validateVersionFormat pkgVersion =
  if isJust $ matchRegex validationRegex pkgVersion
    then Nothing
    else Just . createErrorWithErrorMessage $ _ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . createErrorWithErrorMessage $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

-- --------------------------------
-- HELPERS
-- --------------------------------

heValidateVersionFormat pkgVersion callback =
  case validateVersionFormat pkgVersion of
    Nothing -> callback
    Just error -> return . Left $ error
