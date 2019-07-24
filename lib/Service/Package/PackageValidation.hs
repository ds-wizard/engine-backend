module Service.Package.PackageValidation
  ( validateVersionFormat
  , validateIsVersionHigher
  , validatePackageIdWithCoordinates
  , validatePackageIdUniqueness
  , validatePreviousPackageIdExistence
  , validatePackagesDeletation
  , validatePackageDeletation
  , validateUsageBySomeBranch
  , validateUsageBySomeQuestionnaire
  -- Helpers
  , heValidateVersionFormat
  , heValidateIsVersionHigher
  , heValidatePackageIdWithCoordinates
  , heValidatePackageIdUniqueness
  , heValidatePreviousPackageIdExistence
  , heValidateMaybePreviousPackageIdExistence
  , hmValidateUsageBySomeBranch
  , hmValidateUsageBySomeQuestionnaire
  ) where

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
import Service.Package.PackageMapper
import Service.Package.PackageUtils

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

validatePackageIdWithCoordinates :: String -> String -> String -> String -> Maybe AppError
validatePackageIdWithCoordinates pId organizationId kmId version =
  if pId == buildPackageId organizationId kmId version
    then Nothing
    else Just . createErrorWithErrorMessage $ _ERROR_SERVICE_PKG__PKG_ID_MISMATCH pId

validatePackageIdUniqueness :: String -> AppContextM (Maybe AppError)
validatePackageIdUniqueness pkgId = do
  eitherPackage <- findPackageById pkgId
  case eitherPackage of
    Left (NotExistsError _) -> return Nothing
    Right _ -> return . Just . createErrorWithErrorMessage $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId
    Left error -> return . Just $ error

validatePreviousPackageIdExistence :: String -> String -> AppContextM (Maybe AppError)
validatePreviousPackageIdExistence pkgId previousPkgId = do
  eitherPackage <- findPackageById previousPkgId
  case eitherPackage of
    Right _ -> return Nothing
    Left (NotExistsError _) ->
      return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST previousPkgId pkgId
    Left error -> return . Just $ error

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
      hmValidateUsageBySomeBranch pkgId $ \() ->
        validateUsageBySomeOtherPackage pkgId $ \() -> hmValidateUsageBySomeQuestionnaire pkgId $ \() -> return Nothing
    validateUsageBySomeOtherPackage pkgId callback = do
      eitherPkgs <- findPackagesByPreviousPackageId pkgId
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
  hmValidateUsageBySomeBranch pkgId $ \() ->
    validateUsageBySomeOtherPackage pkgId $ \() -> hmValidateUsageBySomeQuestionnaire pkgId $ \() -> return Nothing
  where
    validateUsageBySomeOtherPackage pkgId callback = do
      eitherPkgs <- findPackagesByPreviousPackageId pkgId
      case eitherPkgs of
        Right [] -> callback ()
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
        Left error -> return . Just $ error

validateUsageBySomeBranch :: String -> AppContextM (Maybe AppError)
validateUsageBySomeBranch pkgId = do
  eitherBranches <- findBranchByPreviousPackageIdOrForkOfPackageIdOrMergeCheckpointPackageId pkgId
  case eitherBranches of
    Right [] -> return Nothing
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "knowledge model"
    Left error -> return . Just $ error

validateUsageBySomeQuestionnaire :: String -> AppContextM (Maybe AppError)
validateUsageBySomeQuestionnaire pkgId = do
  eitherQuestionnaires <- findQuestionnaireByPackageId pkgId
  case eitherQuestionnaires of
    Right [] -> return Nothing
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "questionnaire"
    Left error -> return . Just $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateVersionFormat pkgVersion callback =
  case validateVersionFormat pkgVersion of
    Nothing -> callback
    Just error -> return . Left $ error

-- -----------------------------------------------------
heValidateIsVersionHigher newVersion oldVersion callback =
  case validateIsVersionHigher newVersion oldVersion of
    Nothing -> callback
    Just error -> return . Left $ error

-- -----------------------------------------------------
heValidatePackageIdWithCoordinates pkgId organizationId kmId version callback =
  case validatePackageIdWithCoordinates pkgId organizationId kmId version of
    Nothing -> callback
    Just error -> return . Left $ error

-- -----------------------------------------------------
heValidatePackageIdUniqueness pkgId callback = do
  maybeError <- validatePackageIdUniqueness pkgId
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error

-- -----------------------------------------------------
heValidatePreviousPackageIdExistence pkgId previousPkgId callback = do
  maybeError <- validatePreviousPackageIdExistence pkgId previousPkgId
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error

heValidateMaybePreviousPackageIdExistence pkgId mPreviousPkgId callback =
  case mPreviousPkgId of
    Just previousPkgId -> heValidatePreviousPackageIdExistence pkgId previousPkgId $ callback
    Nothing -> callback

-- -----------------------------------------------------
hmValidateUsageBySomeBranch pkgId callback = do
  maybeError <- validateUsageBySomeBranch pkgId
  case maybeError of
    Nothing -> callback ()
    Just error -> return . Just $ error

-- -----------------------------------------------------
hmValidateUsageBySomeQuestionnaire pkgId callback = do
  maybeError <- validateUsageBySomeQuestionnaire pkgId
  case maybeError of
    Nothing -> callback ()
    Just error -> return . Just $ error
