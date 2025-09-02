module Wizard.Database.DAO.User.UserSubmissionPropDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.User ()
import Wizard.Database.Mapping.User.UserSubmissionProp ()
import Wizard.Database.Mapping.User.UserSubmissionPropList ()
import Wizard.Database.Mapping.User.UserSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserSubmissionProp
import Wizard.Model.User.UserSubmissionPropList
import WizardLib.Public.Database.Mapping.User.UserWithMembership ()

entityName = "user_entity_submission_prop"

findUserSubmissionProps :: U.UUID -> AppContextM [UserSubmissionProp]
findUserSubmissionProps userUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserSubmissionPropsList :: U.UUID -> AppContextM [UserSubmissionPropList]
findUserSubmissionPropsList userUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT nested.id                                                          AS service_id, \
          \       nested.name                                                        AS service_name, \
          \       jsonb_object_agg(nested.key, coalesce((submission_prop.values ->> nested.key)::varchar, '')) AS props \
          \FROM (SELECT service.id, \
          \             service.name, \
          \             service.tenant_uuid, \
          \             unnest(service.props) AS key \
          \      FROM config_submission_service AS service \
          \      WHERE service.tenant_uuid = ?) AS nested \
          \LEFT JOIN user_entity_submission_prop AS submission_prop \
          \          ON submission_prop.service_id = nested.id AND \
          \             submission_prop.tenant_uuid = nested.tenant_uuid AND \
          \             submission_prop.user_uuid = ? \
          \GROUP BY nested.id, nested.name;"
  let params = [U.toString tenantUuid, U.toString userUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertOrUpdateUserSubmissionProp :: UserSubmissionProp -> AppContextM Int64
insertOrUpdateUserSubmissionProp submissionProps = do
  let sql =
        fromString
          "INSERT INTO user_entity_submission_prop \
          \VALUES (?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (user_uuid, service_id, tenant_uuid) DO UPDATE SET user_uuid   = ?, \
          \                                                               service_id  = ?, \
          \                                                               values      = ?, \
          \                                                               tenant_uuid = ?, \
          \                                                               created_at  = ?, \
          \                                                               updated_at  = ?;"
  let params = toRow submissionProps ++ toRow submissionProps
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserSubmissionPropsExcept :: U.UUID -> [String] -> AppContextM Int64
deleteUserSubmissionPropsExcept userUuid serviceIds = do
  let serviceIdCondition =
        case serviceIds of
          [] -> ""
          _ -> f' "AND service_id NOT IN (%s)" [generateQuestionMarks serviceIds]
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "DELETE FROM user_entity_submission_prop \
            \WHERE tenant_uuid = ? AND user_uuid = ? %s"
            [serviceIdCondition]
  let params = U.toString tenantUuid : U.toString userUuid : serviceIds
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserSubmissionProps :: AppContextM Int64
deleteUserSubmissionProps = createDeleteEntitiesFn entityName

deleteUserByUserUuid :: U.UUID -> AppContextM Int64
deleteUserByUserUuid userUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]
