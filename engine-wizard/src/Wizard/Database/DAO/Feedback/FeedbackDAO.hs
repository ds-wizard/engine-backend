module Wizard.Database.DAO.Feedback.FeedbackDAO where

import Control.Lens ((^.))
import Data.Bson

import LensesConfig
import Shared.Database.DAO.Common
import Wizard.Database.BSON.Feedback.Feedback ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextLenses ()
import Wizard.Model.Feedback.Feedback

entityName = "feedback"

collection = "feedbacks"

findFeedbacks :: AppContextM [Feedback]
findFeedbacks = createFindEntitiesFn collection

findFeedbacksFiltered :: [(String, String)] -> AppContextM [Feedback]
findFeedbacksFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findFeedbackById :: String -> AppContextM Feedback
findFeedbackById = createFindEntityByFn collection entityName "uuid"

insertFeedback :: Feedback -> AppContextM Value
insertFeedback = createInsertFn collection

updateFeedbackById :: Feedback -> AppContextM ()
updateFeedbackById feedback = createUpdateByFn collection "uuid" (feedback ^. uuid) feedback

deleteFeedbacks :: AppContextM ()
deleteFeedbacks = createDeleteEntitiesFn collection

deleteFeedbackById :: String -> AppContextM ()
deleteFeedbackById = createDeleteEntityByFn collection "uuid"
