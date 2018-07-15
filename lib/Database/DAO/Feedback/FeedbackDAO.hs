module Database.DAO.Feedback.FeedbackDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Text (Text)
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        rest, save, select)

import Database.BSON.Feedback.Feedback ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Feedback.Feedback

feedbackCollection = "feedbacks"

findFeedbacks :: AppContextM (Either AppError [Feedback])
findFeedbacks = do
  let action = rest =<< find (select [] feedbackCollection)
  feedbacksS <- runDB action
  return . deserializeEntities $ feedbacksS

findFeedbacksFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Feedback])
findFeedbacksFiltered queryParams = do
  let filter = (\(p, v) -> p =: v) <$> queryParams
  let action = rest =<< find (select filter feedbackCollection)
  feedbacksS <- runDB action
  return . deserializeEntities $ feedbacksS

findFeedbackById :: String -> AppContextM (Either AppError Feedback)
findFeedbackById fUuid = do
  let action = findOne $ select ["uuid" =: fUuid] feedbackCollection
  maybeFeedbackS <- runDB action
  return . deserializeMaybeEntity $ maybeFeedbackS

insertFeedback :: Feedback -> AppContextM Value
insertFeedback feedback = do
  let action = insert feedbackCollection (toBSON feedback)
  runDB action

updateFeedbackById :: Feedback -> AppContextM ()
updateFeedbackById feedback = do
  let action =
        fetch (select ["uuid" =: (feedback ^. uuid)] feedbackCollection) >>=
        save feedbackCollection . merge (toBSON feedback)
  runDB action

deleteFeedbacks :: AppContextM ()
deleteFeedbacks = do
  let action = delete $ select [] feedbackCollection
  runDB action

deleteFeedbackById :: String -> AppContextM ()
deleteFeedbackById fUuid = do
  let action = deleteOne $ select ["uuid" =: fUuid] feedbackCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindFeedbacks callback = do
  eitherFeedbacks <- findFeedbacks
  case eitherFeedbacks of
    Right feedbacks -> callback feedbacks
    Left error -> return . Left $ error

hmFindFeedbacks callback = do
  eitherFeedbacks <- findFeedbacks
  case eitherFeedbacks of
    Right feedbacks -> callback feedbacks
    Left error -> return . Just $ error

-- --------------------------------
heFindFeedbacksFiltered queryParams callback = do
  eitherFeedbacks <- findFeedbacksFiltered queryParams
  case eitherFeedbacks of
    Right feedbacks -> callback feedbacks
    Left error -> return . Left $ error

-- --------------------------------
heFindFeedbackById fUuid callback = do
  eitherFeedback <- findFeedbackById fUuid
  case eitherFeedback of
    Right feedback -> callback feedback
    Left error -> return . Left $ error
