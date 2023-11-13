module Wizard.Database.DAO.Submission.SubmissionDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Submission.Submission ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Submission.Submission

entityName = "submission"

pageLabel = "submissions"

findSubmissions :: AppContextM [Submission]
findSubmissions = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findSubmissionsFiltered :: [(String, String)] -> AppContextM [Submission]
findSubmissionsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findSubmissionsByDocumentUuid :: U.UUID -> AppContextM [Submission]
findSubmissionsByDocumentUuid documentUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_uuid", U.toString documentUuid)]

insertSubmission :: Submission -> AppContextM Int64
insertSubmission = createInsertFn entityName

updateSubmissionByUuid :: Submission -> AppContextM Submission
updateSubmissionByUuid sub = do
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  let updatedSub = sub {updatedAt = now}
  let sql =
        fromString
          "UPDATE submission SET uuid = ?, state = ?, location = ?, returned_data = ?, service_id = ?, document_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow sub ++ [toField tenantUuid, toField updatedSub.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedSub

deleteSubmissions :: AppContextM Int64
deleteSubmissions = createDeleteEntitiesFn entityName

deleteSubmissionsFiltered :: [(String, String)] -> AppContextM Int64
deleteSubmissionsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)
