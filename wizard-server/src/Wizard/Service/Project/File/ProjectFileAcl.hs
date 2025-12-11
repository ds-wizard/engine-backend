module Wizard.Service.Project.File.ProjectFileAcl where

import qualified Data.UUID as U

import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import Wizard.Service.Project.ProjectAcl

checkViewPermissionToFile :: U.UUID -> AppContextM ()
checkViewPermissionToFile projectUuid = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions

checkEditPermissionToFile :: U.UUID -> AppContextM ()
checkEditPermissionToFile projectUuid = do
  project <- findProjectByUuid projectUuid
  checkEditPermissionToProject project.visibility project.sharing project.permissions
