module Wizard.Database.DAO.Feedback.FeedbackDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Text (Text)

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper, createHemHelper)
import Wizard.Database.BSON.Feedback.Feedback ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Feedback.Feedback

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
