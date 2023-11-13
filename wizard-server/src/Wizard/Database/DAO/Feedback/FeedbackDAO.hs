module Wizard.Database.DAO.Feedback.FeedbackDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Feedback.Feedback ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Feedback.Feedback

entityName = "feedback"

findFeedbacks :: AppContextM [Feedback]
findFeedbacks = createFindEntitiesFn entityName

findFeedbacksFiltered :: [(String, String)] -> AppContextM [Feedback]
findFeedbacksFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findFeedbackByUuid :: U.UUID -> AppContextM Feedback
findFeedbackByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertFeedback :: Feedback -> AppContextM Int64
insertFeedback = createInsertFn entityName

updateFeedbackByUuid :: Feedback -> AppContextM Int64
updateFeedbackByUuid feedback = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE feedback SET uuid = ?, issue_id = ?, question_uuid = ?, package_id = ?, title = ?, content = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow feedback ++ [toField tenantUuid, toField feedback.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFeedbacks :: AppContextM Int64
deleteFeedbacks = createDeleteEntitiesFn entityName

deleteFeedbackByUuid :: U.UUID -> AppContextM Int64
deleteFeedbackByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
