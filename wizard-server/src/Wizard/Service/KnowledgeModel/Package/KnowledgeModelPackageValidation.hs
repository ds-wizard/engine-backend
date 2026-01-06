module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageAudit

validateIsVersionHigher :: String -> String -> AppContextM ()
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then return ()
    else throwError . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

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

validatePackagesDeletion :: [String] -> AppContextM ()
validatePackagesDeletion pkgIds = forM_ pkgIds validateOnePackage
  where
    validateOnePackage :: String -> AppContextM ()
    validateOnePackage pkgId = do
      validateUsageBySomeKnowledgeModelEditor pkgId
      validateUsageBySomeProject pkgId
      validateUsageBySomeOtherPackage pkgId
    validateUsageBySomeOtherPackage pkgId = do
      pkgs <- findPackagesByForkOfPackageId pkgId
      case pkgs of
        [] -> return ()
        _ -> do
          auditPackageFailedToDeleteDueParentPackages pkgId pkgs
          throwError . UserError $
            _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"

validatePackageDeletion :: String -> AppContextM ()
validatePackageDeletion pkgId = do
  validateUsageBySomeKnowledgeModelEditor pkgId
  validateUsageBySomeProject pkgId
  validateUsageBySomeOtherPackage pkgId
  where
    validateUsageBySomeOtherPackage pkgId = do
      pkgs <- findPackagesByPreviousPackageId pkgId
      case pkgs of
        [] -> do
          pkgs <- findPackagesByForkOfPackageId pkgId
          case pkgs of
            [] -> return ()
            _ -> do
              auditPackageFailedToDeleteDueParentPackages pkgId pkgs
              throwError . UserError $
                _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"
        _ -> do
          auditPackageFailedToDeleteDuePreviousPackages pkgId pkgs
          throwError . UserError $
            _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "package"

validateUsageBySomeKnowledgeModelEditor :: String -> AppContextM ()
validateUsageBySomeKnowledgeModelEditor pkgId = do
  editors <- findKnowledgeModelEditorsByPreviousPackageId pkgId
  case editors of
    [] -> return ()
    _ -> do
      auditPackageFailedToDeleteDueKmEditors pkgId editors
      throwError . UserError $
        _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "knowledge model"

validateUsageBySomeProject :: String -> AppContextM ()
validateUsageBySomeProject pkgId = do
  projects <- findProjectsByKnowledgeModelPackageId pkgId
  case projects of
    [] -> return ()
    _ -> do
      auditPackageFailedToDeleteDueProjects pkgId projects
      throwError . UserError $
        _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId "project"
