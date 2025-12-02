module Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnairePerm ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm

entityName_user = "questionnaire_perm_user"

entityName_group = "questionnaire_perm_group"

findQuestionnairePermsFiltered :: [(String, String)] -> AppContextM [QuestionnairePerm]
findQuestionnairePermsFiltered queryParams = do
  let sql =
        fromString $
          f'
            "SELECT questionnaire_uuid, 'UserQuestionnairePermType' AS member_type, user_uuid as member_uuid, perms, tenant_uuid \
            \ FROM %s \
            \ WHERE %s \
            \ UNION \
            \ SELECT questionnaire_uuid, 'UserGroupQuestionnairePermType' AS member_type, user_group_uuid as member_uuid, perms, tenant_uuid \
            \ FROM %s \
            \ WHERE %s"
            [entityName_user, mapToDBQuerySql queryParams, entityName_group, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams ++ fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertQuestionnairePerm :: QuestionnairePerm -> AppContextM Int64
insertQuestionnairePerm perm =
  case perm.memberType of
    UserQuestionnairePermType -> createInsertFn entityName_user perm
    UserGroupQuestionnairePermType -> createInsertFn entityName_group perm

deleteQuestionnairePerms :: AppContextM Int64
deleteQuestionnairePerms = do
  createDeleteEntitiesFn entityName_user
  createDeleteEntitiesFn entityName_group

deleteQuestionnairePermsFiltered :: [(String, String)] -> AppContextM Int64
deleteQuestionnairePermsFiltered queryParams = do
  createDeleteEntitiesByFn entityName_user queryParams
  createDeleteEntitiesByFn entityName_group queryParams

deleteQuestionnairePermGroupByUserGroupUuid :: U.UUID -> AppContextM Int64
deleteQuestionnairePermGroupByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName_group [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)]
