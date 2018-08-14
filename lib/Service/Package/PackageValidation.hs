module Service.Package.PackageValidation where

import Control.Lens ((^.))
import Data.Maybe
import Text.Regex

import Database.DAO.Branch.BranchDAO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.Package.Package
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
      validateUsageBySomeBranch pkgId $ \() ->
        validateUsageBySomeOtherPackage pkgId $ \() -> validateUsageBySomeQuestionnaire pkgId $ \() -> return Nothing
    validateUsageBySomeOtherPackage pkgId callback = do
      eitherPkgs <- findPackagesByParentPackageId pkgId
      case eitherPkgs of
        Right [] -> callback ()
        Right pkgs -> do
          if length (filter (filFun) pkgs) > 0
            then return . Just . createErrorWithErrorMessage $
                 _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
            else callback ()
        Left error -> return . Just $ error
      where
        filFun :: Package -> Bool
        filFun p = not ((p ^. pId) `elem` pkgPIdsToDelete)

validatePackageDeletation :: String -> AppContextM (Maybe AppError)
validatePackageDeletation pkgId =
  validateUsageBySomeBranch pkgId $ \() ->
    validateUsageBySomeOtherPackage pkgId $ \() -> validateUsageBySomeQuestionnaire pkgId $ \() -> return Nothing
  where
    validateUsageBySomeOtherPackage pkgId callback = do
      eitherPkgs <- findPackagesByParentPackageId pkgId
      case eitherPkgs of
        Right [] -> callback ()
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
        Left error -> return . Just $ error

validateUsageBySomeBranch pkgId callback = do
  eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId pkgId
  case eitherBranches of
    Right [] -> callback ()
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "knowledge model"
    Left error -> return . Just $ error

validateUsageBySomeQuestionnaire pkgId callback = do
  eitherQuestionnaires <- findQuestionnaireByPackageId pkgId
  case eitherQuestionnaires of
    Right [] -> callback ()
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "questionnaire"
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
