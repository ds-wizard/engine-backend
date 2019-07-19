module Database.DAO.Feedback.FeedbackDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Text (Text)

import Database.BSON.Feedback.Feedback ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Feedback.Feedback
import Util.Helper (createHeeHelper, createHemHelper)

entityName = "feedback"

collection = "feedbacks"

findFeedbacks :: AppContextM (Either AppError [Feedback])
findFeedbacks = createFindEntitiesFn collection

findFeedbacksFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Feedback])
findFeedbacksFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findFeedbackById :: String -> AppContextM (Either AppError Feedback)
findFeedbackById = createFindEntityByFn collection entityName "uuid"

insertFeedback :: Feedback -> AppContextM Value
insertFeedback = createInsertFn collection

updateFeedbackById :: Feedback -> AppContextM ()
updateFeedbackById feedback = createUpdateByFn collection "uuid" (feedback ^. uuid) feedback

deleteFeedbacks :: AppContextM ()
deleteFeedbacks = createDeleteEntitiesFn collection

deleteFeedbackById :: String -> AppContextM ()
deleteFeedbackById = createDeleteEntityByFn collection "uuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindFeedbacks callback = createHeeHelper findFeedbacks callback

hmFindFeedbacks callback = createHemHelper findFeedbacks callback

-- --------------------------------
heFindFeedbacksFiltered queryParams callback = createHeeHelper (findFeedbacksFiltered queryParams) callback

-- --------------------------------
heFindFeedbackById fUuid callback = createHeeHelper (findFeedbackById fUuid) callback
