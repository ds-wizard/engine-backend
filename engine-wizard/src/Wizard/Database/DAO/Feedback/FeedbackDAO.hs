module Wizard.Database.DAO.Feedback.FeedbackDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.String
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
findFeedbacksFiltered params = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName (appQueryUuid appUuid : params)

findFeedbackById :: String -> AppContextM Feedback
findFeedbackById uuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertFeedback :: Feedback -> AppContextM Int64
insertFeedback = createInsertFn entityName

updateFeedbackById :: Feedback -> AppContextM Int64
updateFeedbackById feedback = do
  appUuid <- asks _appContextAppUuid
  let params = toRow feedback ++ [toField appUuid, toField $ feedback ^. uuid]
  let sql =
        "UPDATE feedback SET uuid = ?, issue_id = ?, question_uuid = ?, package_id = ?, title = ?, content = ?, created_at = ?, updated_at = ?, app_uuid = ? WHERE app_uuid = ? AND uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteFeedbacks :: AppContextM Int64
deleteFeedbacks = createDeleteEntitiesFn entityName

deleteFeedbackById :: String -> AppContextM Int64
deleteFeedbackById uuid = do
  appUuid <- asks _appContextAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]
