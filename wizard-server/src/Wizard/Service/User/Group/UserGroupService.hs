module Wizard.Service.User.Group.UserGroupService where

import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectPermDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserGroupDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.ProjectSimpleWithPerm
import Wizard.Model.User.UserGroupSuggestion
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Service.User.Group.UserGroupAcl
import Wizard.Service.User.Group.UserGroupMapper
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Model.User.UserWithMembership
import WizardLib.Public.Service.User.Group.UserGroupMapper

getUserGroupSuggestions :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserGroupSuggestion)
getUserGroupSuggestions = findUserGroupSuggestionsPage

createUserGroup :: U.UUID -> String -> Maybe String -> Bool -> AppContextM ()
createUserGroup uuid name description private = do
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let userGroup = fromCreate uuid name description private tenantUuid now
  insertUserGroup userGroup
  return ()

getUserGroupByUuid :: U.UUID -> AppContextM UserGroupDetailDTO
getUserGroupByUuid uuid = do
  userGroup <- findUserGroupByUuid uuid
  users <- findUsersByUserGroupUuid uuid
  checkViewPermission userGroup users
  return $ toDetailDTO userGroup users

modifyUserGroup :: U.UUID -> String -> Maybe String -> Bool -> AppContextM ()
modifyUserGroup uuid name description private = do
  userGroup <- findUserGroupByUuid uuid
  now <- liftIO getCurrentTime
  let updatedUserGroup = fromChange userGroup name description private now
  updateUserGroupByUuid updatedUserGroup
  return ()

deleteUserGroup :: U.UUID -> AppContextM ()
deleteUserGroup userGroupUuid =
  runInTransaction $ do
    -- 1. Recompute all project permissions for websockets
    projects <- findProjectsSimpleWithPermByUserGroupUuid userGroupUuid
    let projectsWithoutUserGroup = fmap (\project -> project {permissions = filter (\projectPerm -> projectPerm.memberUuid /= userGroupUuid) project.permissions}) projects
    traverse_ (\project -> updatePermsForOnlineUsers project.uuid project.visibility project.sharing project.permissions) projectsWithoutUserGroup
    -- 2. Delete project perm group
    deleteProjectPermGroupByUserGroupUuid userGroupUuid
    -- 3. Delete user group memberships
    deleteUserGroupMembershipsByUserGroupUuid userGroupUuid
    -- 4. Delete user group
    deleteUserGroupByUuid userGroupUuid
