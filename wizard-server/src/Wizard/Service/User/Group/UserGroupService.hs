module Wizard.Service.User.Group.UserGroupService where

import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO
import Wizard.Database.DAO.User.UserGroupDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireSimpleWithPerm
import Wizard.Model.User.UserGroupSuggestion
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.User.Group.UserGroupMapper
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO

getUserGroupSuggestions :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserGroupSuggestion)
getUserGroupSuggestions = findUserGroupSuggestionsPage

createUserGroup :: U.UUID -> String -> Maybe String -> Bool -> AppContextM ()
createUserGroup uuid name description private = do
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let userGroup = fromCreate uuid name description private tenantUuid now
  insertUserGroup userGroup
  return ()

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
    -- 1. Recompute all questionnaire permissions for websockets
    questionnaires <- findQuestionnairesSimpleWithPermByUserGroupUuid userGroupUuid
    let questionnairesWithoutUserGroup = fmap (\qtn -> qtn {permissions = filter (\qtnPerm -> qtnPerm.memberUuid /= userGroupUuid) qtn.permissions}) questionnaires
    traverse_ (\qtn -> updatePermsForOnlineUsers qtn.uuid qtn.visibility qtn.sharing qtn.permissions) questionnairesWithoutUserGroup
    -- 2. Delete questionnaire perm group
    deleteQuestionnairePermGroupByUserGroupUuid userGroupUuid
    -- 3. Delete user group memberships
    deleteUserGroupMembershipsByUserGroupUuid userGroupUuid
    -- 4. Delete user group
    deleteUserGroupByUuid userGroupUuid
