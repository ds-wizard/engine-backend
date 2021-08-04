module Wizard.Database.DAO.Feedback.FeedbackDAO where

import Control.Lens ((^.))
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Feedback.Feedback ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Feedback.Feedback
import Wizard.Util.Logger

entityName = "feedback"

findFeedbacks :: AppContextM [Feedback]
findFeedbacks = createFindEntitiesFn entityName

findFeedbacksFiltered :: [(String, String)] -> AppContextM [Feedback]
findFeedbacksFiltered = createFindEntitiesByFn entityName

findFeedbackById :: String -> AppContextM Feedback
findFeedbackById = createFindEntityByFn entityName "uuid"

insertFeedback :: Feedback -> AppContextM Int64
insertFeedback = createInsertFn entityName

updateFeedbackById :: Feedback -> AppContextM Int64
updateFeedbackById feedback = do
  let params = toRow feedback ++ [toField . U.toText $ feedback ^. uuid]
  let sql =
        "UPDATE feedback SET uuid = ?, issue_id = ?, question_uuid = ?, package_id = ?, title = ?, content = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteFeedbacks :: AppContextM Int64
deleteFeedbacks = createDeleteEntitiesFn entityName

deleteFeedbackById :: String -> AppContextM Int64
deleteFeedbackById = createDeleteEntityByFn entityName "uuid"
