module Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO where

import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVersion ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVersionList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Questionnaire.QuestionnaireVersionList

entityName = "questionnaire_version"

pageLabel = "questionnaireVersions"

findQuestionnaireVersionsByQuestionnaireUuid :: U.UUID -> AppContextM [QuestionnaireVersion]
findQuestionnaireVersionsByQuestionnaireUuid qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsByFn "*" entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString qtnUuid)]

findQuestionnaireVersionListByQuestionnaireUuidAndCreatedAt :: U.UUID -> Maybe UTCTime -> AppContextM [QuestionnaireVersionList]
findQuestionnaireVersionListByQuestionnaireUuidAndCreatedAt questionnaireUuid mCreatedAt = do
  tenantUuid <- asks currentTenantUuid
  let (createdAtCondition, createdAtParams) =
        case mCreatedAt of
          Just createdAt -> ("AND v.created_at <= ?", [toField createdAt])
          Nothing -> ("", [])
  let sql =
        fromString $
          f''
            "SELECT v.uuid, \
            \       v.name, \
            \       v.description, \
            \       v.event_uuid, \
            \       v.created_at, \
            \       v.updated_at, \
            \       u.uuid, \
            \       u.first_name, \
            \       u.last_name, \
            \       u.email, \
            \       u.image_url \
            \FROM questionnaire_version v \
            \JOIN user_entity u ON u.uuid = v.created_by AND u.tenant_uuid = v.tenant_uuid ${createdAtCondition} \
            \WHERE v.tenant_uuid = ? AND v.questionnaire_uuid = ? \
            \ORDER BY v.created_at"
            [("createdAtCondition", createdAtCondition)]
  let params = createdAtParams ++ [toField tenantUuid, toField questionnaireUuid]
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findQuestionnaireVersionByUuid :: U.UUID -> AppContextM QuestionnaireVersion
findQuestionnaireVersionByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn "*" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findQuestionnaireVersionByEventUuid' :: U.UUID -> U.UUID -> AppContextM (Maybe QuestionnaireVersion)
findQuestionnaireVersionByEventUuid' questionnaireUuid eventUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString questionnaireUuid), ("event_uuid", U.toString eventUuid)]

insertQuestionnaireVersion :: QuestionnaireVersion -> AppContextM Int64
insertQuestionnaireVersion = createInsertFn entityName

updateQuestionnaireVersionByUuid :: QuestionnaireVersion -> AppContextM Int64
updateQuestionnaireVersionByUuid version = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE questionnaire_version SET uuid = ?, name = ?, description = ?, event_uuid = ?, questionnaire_uuid = ?, tenant_uuid = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow version ++ [toField version.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

clearQuestionnaireVersionCreatedBy :: U.UUID -> AppContextM ()
clearQuestionnaireVersionCreatedBy userUuid = do
  let sql = fromString "UPDATE questionnaire_version SET created_by = null WHERE created_by = ?"
  let params = [U.toString userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

deleteQuestionnaireVersions :: AppContextM Int64
deleteQuestionnaireVersions = createDeleteEntitiesFn entityName

deleteQuestionnaireVersionsByQuestionnaireUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireVersionsByQuestionnaireUuid questionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString questionnaireUuid)]

deleteQuestionnaireVersionsByUuids :: [U.UUID] -> AppContextM ()
deleteQuestionnaireVersionsByUuids versionUuids =
  unless
    (null versionUuids)
    (void $ createDeleteEntityWhereInFn entityName "uuid" (fmap U.toString versionUuids))

deleteQuestionnaireVersionByUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireVersionByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
