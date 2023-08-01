module Wizard.Database.DAO.Questionnaire.QuestionnaireAclDAO where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl ()
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireAcl

entityName_user = "questionnaire_acl_user"

entityName_group = "questionnaire_acl_group"

findQuestionnairePermRecordsFiltered :: [(String, String)] -> AppContextM [QuestionnairePermRecord]
findQuestionnairePermRecordsFiltered queryParams = do
  let sql =
        fromString $
          f'
            "SELECT uuid, 'UserMember' AS member_type, text(user_uuid) AS member_id, perms, questionnaire_uuid \
            \ FROM %s \
            \ WHERE %s \
            \ UNION \
            \ SELECT uuid, 'GroupMember' AS member_type, group_id AS member_id, perms, questionnaire_uuid \
            \ FROM %s \
            \ WHERE %s"
            [entityName_user, mapToDBQuerySql queryParams, entityName_group, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams ++ fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertQuestionnairePermRecord :: QuestionnairePermRecord -> AppContextM Int64
insertQuestionnairePermRecord perm@QuestionnairePermRecord {..} =
  case member of
    m@GroupMember {..} -> createInsertFn entityName_group perm
    m@UserMember {..} -> createInsertFn entityName_user perm

deleteQuestionnairePermRecords :: AppContextM Int64
deleteQuestionnairePermRecords = do
  createDeleteEntitiesFn entityName_user
  createDeleteEntitiesFn entityName_group

deleteQuestionnairePermRecordsFiltered :: [(String, String)] -> AppContextM Int64
deleteQuestionnairePermRecordsFiltered queryParams = do
  createDeleteEntitiesByFn entityName_user queryParams
  createDeleteEntitiesByFn entityName_group queryParams
