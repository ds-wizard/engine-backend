module Wizard.Service.Document.DocumentAcl where

import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Project
import Wizard.Service.Project.ProjectAcl

checkViewPermissionToDoc :: Maybe U.UUID -> AppContextM ()
checkViewPermissionToDoc mProjectUuid = do
  case mProjectUuid of
    Just projectUuid -> do
      project <- findProjectByUuid projectUuid
      checkViewPermissionToProject project.visibility project.sharing project.permissions
    Nothing -> throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Read Document"

checkViewPermissionToDoc' :: Project -> AppContextM ()
checkViewPermissionToDoc' project = checkViewPermissionToProject project.visibility project.sharing project.permissions

checkEditPermissionToDoc :: Maybe U.UUID -> AppContextM ()
checkEditPermissionToDoc mProjectUuid = do
  case mProjectUuid of
    Just projectUuid -> do
      _ <- getCurrentUser
      project <- findProjectByUuid projectUuid
      checkEditPermissionToProject project.visibility project.sharing project.permissions
    Nothing -> throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Document"
