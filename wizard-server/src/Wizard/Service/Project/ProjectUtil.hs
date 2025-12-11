module Wizard.Service.Project.ProjectUtil where

import Control.Monad (when)
import qualified Data.UUID as U

import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Event.ProjectEventLenses ()
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectState
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.Project.ProjectMapper
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Database.DAO.User.UserGroupDAO

extractVisibility dto = do
  tcProject <- getCurrentTenantConfigProject
  if tcProject.projectVisibility.enabled
    then return dto.visibility
    else return $ tcProject.projectVisibility.defaultValue

extractSharing dto = do
  tcProject <- getCurrentTenantConfigProject
  if tcProject.projectSharing.enabled
    then return dto.sharing
    else return $ tcProject.projectSharing.defaultValue

enhanceProjectPerm :: ProjectPerm -> AppContextM ProjectPermDTO
enhanceProjectPerm projectPerm =
  case projectPerm.memberType of
    UserProjectPermType -> do
      user <- findUserByUuid projectPerm.memberUuid
      return $ toUserProjectPermDTO projectPerm user
    UserGroupProjectPermType -> do
      userGroup <- findUserGroupByUuid projectPerm.memberUuid
      return $ toUserGroupProjectPermDTO projectPerm userGroup

getProjectState :: U.UUID -> String -> AppContextM ProjectState
getProjectState projectUuid pkgId = do
  mMs <- findProjectMigrationByNewProjectUuid' projectUuid
  case mMs of
    Just _ -> return MigratingProjectState
    Nothing -> do
      pkgs <- getNewerPackages pkgId True
      if null pkgs
        then return DefaultProjectState
        else return OutdatedProjectState

skipIfAssigningProject :: Project -> AppContextM () -> AppContextM ()
skipIfAssigningProject project action = do
  tcProject <- getCurrentTenantConfigProject
  let projectSharingEnabled = tcProject.projectSharing.enabled
  let projectSharingAnonymousEnabled = tcProject.projectSharing.anonymousEnabled
  when
    (not (projectSharingEnabled && projectSharingAnonymousEnabled) || (not . null $ project.permissions))
    action
