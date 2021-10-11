module Wizard.Database.DAO.Submission.SubmissionDAO where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Submission.Submission ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Submission.Submission
import Wizard.Util.Logger

entityName = "submission"

pageLabel = "submissions"

findSubmissions :: AppContextM [Submission]
findSubmissions = createFindEntitiesFn entityName

findSubmissionsFiltered :: [(String, String)] -> AppContextM [Submission]
findSubmissionsFiltered = createFindEntitiesByFn entityName

findSubmissionsByDocumentUuid :: String -> AppContextM [Submission]
findSubmissionsByDocumentUuid templateId = createFindEntitiesByFn entityName [("document_uuid", templateId)]

findSubmissionById :: String -> AppContextM Submission
findSubmissionById = createFindEntityByFn entityName "uuid"

insertSubmission :: Submission -> AppContextM Int64
insertSubmission = createInsertFn entityName

updateSubmissionById :: Submission -> AppContextM Submission
updateSubmissionById sub = do
  now <- liftIO getCurrentTime
  let updatedSub = sub & updatedAt .~ now
  let params = toRow sub ++ [toField . U.toText $ updatedSub ^. uuid]
  let sql =
        "UPDATE submission SET uuid = ?, state = ?, location = ?, returned_data = ?, service_id = ?, document_uuid = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action
  return updatedSub

deleteSubmissions :: AppContextM Int64
deleteSubmissions = createDeleteEntitiesFn entityName

deleteSubmissionsFiltered :: [(String, String)] -> AppContextM Int64
deleteSubmissionsFiltered = createDeleteEntitiesByFn entityName

deleteSubmissionById :: String -> AppContextM Int64
deleteSubmissionById = createDeleteEntityByFn entityName "uuid"
