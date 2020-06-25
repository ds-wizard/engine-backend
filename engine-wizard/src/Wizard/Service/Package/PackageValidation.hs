module Wizard.Service.Package.PackageValidation
  ( validatePackageIdFormat
  , validateVersionFormat
  , validateIsVersionHigher
  , validatePackageIdWithCoordinates
  , validatePackageIdUniqueness
  , validatePreviousPackageIdExistence
  , validateMaybePreviousPackageIdExistence
  , validatePackagesDeletation
  , validatePackageDeletation
  , validateUsageBySomeBranch
  , validateUsageBySomeQuestionnaire
  ) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Data.Maybe
import Text.Regex

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Util.IdentifierUtil

validatePackageIdFormat :: String -> AppContextM ()
validatePackageIdFormat pkgId =
  let pkgIdSplit = splitPackageId pkgId
   in if length pkgIdSplit /= 3 || null (head pkgIdSplit) || null (pkgIdSplit !! 1)
        then throwError . UserError $ _ERROR_VALIDATION__INVALID_PKG_ID_FORMAT
        else validateVersionFormat (pkgIdSplit !! 2)

validateVersionFormat :: String -> AppContextM ()
validateVersionFormat pkgVersion =
  if isJust $ matchRegex validationRegex pkgVersion
    then return ()
    else throwError . UserError $ _ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

validateIsVersionHigher :: String -> String -> AppContextM ()
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then return ()
    else throwError . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

validatePackageIdWithCoordinates :: String -> String -> String -> String -> AppContextM ()
validatePackageIdWithCoordinates pId organizationId kmId version =
  if pId == buildPackageId organizationId kmId version
    then return ()
    else throwError . UserError $ _ERROR_SERVICE_PKG__PKG_ID_MISMATCH pId

validatePackageIdUniqueness :: String -> AppContextM ()
validatePackageIdUniqueness pkgId = do
  mPkg <- findPackageById' pkgId
  case mPkg of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId

validatePreviousPackageIdExistence :: String -> String -> AppContextM ()
validatePreviousPackageIdExistence pkgId previousPkgId = do
  mPkg <- findPackageById' previousPkgId
  case mPkg of
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST previousPkgId pkgId

validateMaybePreviousPackageIdExistence :: String -> Maybe String -> AppContextM ()
validateMaybePreviousPackageIdExistence pkgId mPreviousPkgId =
  forM_ mPreviousPkgId (validatePreviousPackageIdExistence pkgId)

validatePackagesDeletation :: [String] -> AppContextM ()
validatePackagesDeletation pkgIds = forM_ pkgIds validateOnePackage
  where
    validateOnePackage :: String -> AppContextM ()
    validateOnePackage pkgId = do
      validateUsageBySomeBranch pkgId
      validateUsageBySomeQuestionnaire pkgId
      validateUsageBySomeOtherPackage pkgId
    validateUsageBySomeOtherPackage pkgId = do
      pkgs <- findPackagesByForkOfPackageId pkgId
      case pkgs of
        [] -> return ()
        _ ->
          throwError . UserError $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"

validatePackageDeletation :: String -> AppContextM ()
validatePackageDeletation pkgId = do
  validateUsageBySomeBranch pkgId
  validateUsageBySomeQuestionnaire pkgId
  validateUsageBySomeOtherPackage pkgId
  where
    validateUsageBySomeOtherPackage pkgId = do
      pkgs <- findPackagesByPreviousPackageId pkgId
      case pkgs of
        [] -> do
          pkgs <- findPackagesByForkOfPackageId pkgId
          case pkgs of
            [] -> return ()
            _ ->
              throwError . UserError $
              _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
        _ ->
          throwError . UserError $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"

validateUsageBySomeBranch :: String -> AppContextM ()
validateUsageBySomeBranch pkgId = do
  branches <- findBranchesByPreviousPackageId pkgId
  case branches of
    [] -> return ()
    _ ->
      throwError . UserError $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "knowledge model"

validateUsageBySomeQuestionnaire :: String -> AppContextM ()
validateUsageBySomeQuestionnaire pkgId = do
  questionnaires <- findQuestionnaireByPackageId pkgId
  case questionnaires of
    [] -> return ()
    _ ->
      throwError . UserError $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "questionnaire"
