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
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findSubmissionsFiltered :: [(String, String)] -> AppContextM [Submission]
findSubmissionsFiltered params = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName (appQueryUuid appUuid : params)

findSubmissionsByDocumentUuid :: U.UUID -> AppContextM [Submission]
findSubmissionsByDocumentUuid documentUuid = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_uuid", U.toString documentUuid)]

findSubmissionById :: String -> AppContextM Submission
findSubmissionById uuid = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertSubmission :: Submission -> AppContextM Int64
insertSubmission = createInsertFn entityName

updateSubmissionById :: Submission -> AppContextM Submission
updateSubmissionById sub = do
  now <- liftIO getCurrentTime
  appUuid <- asks currentAppUuid
  let updatedSub = sub {updatedAt = now}
  let sql =
        fromString
          "UPDATE submission SET uuid = ?, state = ?, location = ?, returned_data = ?, service_id = ?, document_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, app_uuid = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow sub ++ [toField appUuid, toField updatedSub.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedSub

deleteSubmissions :: AppContextM Int64
deleteSubmissions = createDeleteEntitiesFn entityName

deleteSubmissionsFiltered :: [(String, String)] -> AppContextM Int64
deleteSubmissionsFiltered params = do
  appUuid <- asks currentAppUuid
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : params)

deleteSubmissionById :: String -> AppContextM Int64
deleteSubmissionById uuid = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]
