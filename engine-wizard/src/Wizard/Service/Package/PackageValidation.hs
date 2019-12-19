module Wizard.Service.Package.PackageValidation
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

import Data.Maybe
import Text.Regex

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageUtils

validateVersionFormat :: String -> Maybe AppError
validateVersionFormat pkgVersion =
  if isJust $ matchRegex validationRegex pkgVersion
    then Nothing
    else Just $ UserError _ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

validatePackageIdWithCoordinates :: String -> String -> String -> String -> Maybe AppError
validatePackageIdWithCoordinates pId organizationId kmId version =
  if pId == buildPackageId organizationId kmId version
    then Nothing
    else Just . UserError $ _ERROR_SERVICE_PKG__PKG_ID_MISMATCH pId

validatePackageIdUniqueness :: String -> AppContextM (Maybe AppError)
validatePackageIdUniqueness pkgId = do
  eitherPackage <- findPackageById pkgId
  case eitherPackage of
    Left (NotExistsError _) -> return Nothing
    Right _ -> return . Just . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId
    Left error -> return . Just $ error

validatePreviousPackageIdExistence :: String -> String -> AppContextM (Maybe AppError)
validatePreviousPackageIdExistence pkgId previousPkgId = do
  eitherPackage <- findPackageById previousPkgId
  case eitherPackage of
    Right _ -> return Nothing
    Left (NotExistsError _) ->
      return . Just . UserError $ _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST previousPkgId pkgId
    Left error -> return . Just $ error

validatePackagesDeletation :: [String] -> AppContextM (Maybe AppError)
validatePackagesDeletation pkgIds = foldl validateOnePackage (return Nothing) pkgIds
  where
    validateOnePackage :: AppContextM (Maybe AppError) -> String -> AppContextM (Maybe AppError)
    validateOnePackage accIO pkgId = do
      acc <- accIO
      case acc of
        Just error -> return . Just $ error
        Nothing ->
          hmValidateUsageBySomeBranch pkgId $
          hmValidateUsageBySomeQuestionnaire pkgId $ hmValidateUsageBySomeOtherPackage pkgId
    hmValidateUsageBySomeOtherPackage pkgId = do
      ePkgs <- findPackagesByForkOfPackageId pkgId
      case ePkgs of
        Right [] -> return Nothing
        Right _ ->
          return . Just . UserError $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
        Left error -> return . Just $ error

validatePackageDeletation :: String -> AppContextM (Maybe AppError)
validatePackageDeletation pkgId =
  hmValidateUsageBySomeBranch pkgId $ hmValidateUsageBySomeQuestionnaire pkgId $ hmValidateUsageBySomeOtherPackage pkgId
  where
    hmValidateUsageBySomeOtherPackage pkgId = do
      eitherPkgs <- findPackagesByPreviousPackageId pkgId
      case eitherPkgs of
        Right [] -> do
          ePkgs <- findPackagesByForkOfPackageId pkgId
          case ePkgs of
            Right [] -> return Nothing
            Right _ ->
              return . Just . UserError $
              _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
            Left error -> return . Just $ error
        Right _ ->
          return . Just . UserError $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
        Left error -> return . Just $ error

validateUsageBySomeBranch :: String -> AppContextM (Maybe AppError)
validateUsageBySomeBranch pkgId = do
  eitherBranches <- findBranchesByPreviousPackageId pkgId
  case eitherBranches of
    Right [] -> return Nothing
    Right _ ->
      return . Just . UserError $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "knowledge model"
    Left error -> return . Just $ error

validateUsageBySomeQuestionnaire :: String -> AppContextM (Maybe AppError)
validateUsageBySomeQuestionnaire pkgId = do
  eitherQuestionnaires <- findQuestionnaireByPackageId pkgId
  case eitherQuestionnaires of
    Right [] -> return Nothing
    Right _ ->
      return . Just . UserError $
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
    Nothing -> callback
    Just error -> return . Just $ error

-- -----------------------------------------------------
hmValidateUsageBySomeQuestionnaire pkgId callback = do
  maybeError <- validateUsageBySomeQuestionnaire pkgId
  case maybeError of
    Nothing -> callback
    Just error -> return . Just $ error
