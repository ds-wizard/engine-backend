module Wizard.Service.User.GroupMembership.UserGroupMembershipService where

import Control.Monad (unless)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.List
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimpleWithPerm
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.User.GroupMembership.UserGroupMembershipMapper
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Model.PersistentCommand.User.UpdateUserGroupMembershipCommand
import WizardLib.Public.Model.User.UserGroup
import WizardLib.Public.Model.User.UserGroupMembership

modifyUserGroupMemberships :: UpdateUserGroupMembershipCommand -> AppContextM ()
modifyUserGroupMemberships command =
  runInTransaction $ do
    userGroup <- findUserGroupByUuid command.userGroupUuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    memberships <- findUserGroupMembershipsByUserGroupUuid command.userGroupUuid
    let membershipsDb = fmap toCreateDTO memberships
    let membershipsToDelete = membershipsDb L.\\ command.members
    let membershipsToAdd = command.members L.\\ membershipsDb
    let membershipToEdit = groupBy' (.userUuid) (.userUuid) memberships command.members
    traverse_ insertUserGroupMembership . fmap (\dto -> fromCreateDTO dto command.userGroupUuid tenantUuid now) $ membershipsToAdd
    traverse_ updateUserGroupMembershipByUuid . fmap (\(membership, dto) -> fromChangeDTO membership dto now) $ membershipToEdit
    unless (null membershipsToDelete) (deleteUserGroupMembership command.userGroupUuid (fmap (.userUuid) membershipsToDelete))
    updateUserGroupByUuid $ userGroup {updatedAt = now}
    return ()

deleteUserGroupMembership :: U.UUID -> [U.UUID] -> AppContextM ()
deleteUserGroupMembership userGroupUuid userUuids = do
  -- 1. Delete memberships
  deleteUserGroupMembershipsByUserGroupUuidAndUserUuids userGroupUuid userUuids
  -- 2. Remove user group from cached websocket records
  removeUserGroupFromUsers userGroupUuid userUuids
  -- 3. Recompute all questionnaire permissions for websockets
  questionnaires <- findQuestionnairesSimpleWithPermByUserGroupUuid userGroupUuid
  traverse_ (\qtn -> updatePermsForOnlineUsers qtn.uuid qtn.visibility qtn.sharing qtn.permissions) questionnaires
