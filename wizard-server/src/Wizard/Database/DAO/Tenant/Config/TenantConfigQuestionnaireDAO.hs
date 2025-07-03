module Wizard.Database.DAO.Tenant.Config.TenantConfigQuestionnaireDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigQuestionnaire ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "config_questionnaire"

findTenantConfigQuestionnaire :: AppContextM TenantConfigQuestionnaire
findTenantConfigQuestionnaire = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigQuestionnaireByUuid tenantUuid

findTenantConfigQuestionnaireByUuid :: U.UUID -> AppContextM TenantConfigQuestionnaire
findTenantConfigQuestionnaireByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigQuestionnaire :: TenantConfigQuestionnaire -> AppContextM Int64
insertTenantConfigQuestionnaire = createInsertFn entityName

updateTenantConfigQuestionnaire :: TenantConfigQuestionnaire -> AppContextM Int64
updateTenantConfigQuestionnaire config = do
  let sql =
        fromString
          "UPDATE config_questionnaire \
          \SET tenant_uuid = ?, \
          \    visibility_enabled = ?, \
          \    visibility_default_value = ?, \
          \    sharing_enabled = ?, \
          \    sharing_default_value = ?, \
          \    sharing_anonymous_enabled = ?, \
          \    creation = ?, \
          \    project_tagging_enabled = ?, \
          \    project_tagging_tags = ?, \
          \    summary_report = ?, \
          \    feedback_enabled = ?, \
          \    feedback_token = ?, \
          \    feedback_owner = ?, \
          \    feedback_repo = ?, \
          \    created_at = ?, \
          \    updated_at = ? \
          \WHERE tenant_uuid = ?;"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigQuestionnaires :: AppContextM Int64
deleteTenantConfigQuestionnaires = createDeleteEntitiesFn entityName
