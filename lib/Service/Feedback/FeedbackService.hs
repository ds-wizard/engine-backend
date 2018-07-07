module Service.Feedback.FeedbackService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import Data.Text (Text)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import Common.Error
import Common.Uuid
import Database.DAO.Feedback.FeedbackDAO
import LensesConfig
import Model.Context.AppContext
import Service.Feedback.Connector.Connector
import Service.Feedback.Connector.GitHub.GitHubConnector ()
import Service.Feedback.FeedbackMapper

getFeedbacksFiltered :: [(Text, Text)] -> AppContextM (Either AppError [FeedbackDTO])
getFeedbacksFiltered queryParams =
  heFindFeedbacksFiltered queryParams $ \feedbacks -> return . Right $ toDTO <$> feedbacks

createFeedback :: FeedbackCreateDTO -> AppContextM (Either AppError FeedbackDTO)
createFeedback reqDto = do
  fUuid <- liftIO generateUuid
  createFeedbackWithGivenUuid fUuid reqDto

createFeedbackWithGivenUuid :: U.UUID -> FeedbackCreateDTO -> AppContextM (Either AppError FeedbackDTO)
createFeedbackWithGivenUuid fUuid reqDto =
  heCreateIssue (reqDto ^. packageId) (reqDto ^. questionUuid) (reqDto ^. title) (reqDto ^. content) $ \issueId -> do
    now <- liftIO getCurrentTime
    let feedback = fromCreateDTO reqDto fUuid issueId now
    insertFeedback feedback
    return . Right $ toDTO feedback

getFeedbackByUuid :: String -> AppContextM (Either AppError FeedbackDTO)
getFeedbackByUuid fUuid = heFindFeedbackById fUuid $ \feedback -> return . Right $ toDTO feedback

synchronizeFeedbacks :: AppContextM (Maybe AppError)
synchronizeFeedbacks =
  hmGetIssues $ \issues ->
    hmFindFeedbacks $ \feedbacks -> do
      sequence $ (updateOrDeleteFeedback issues) <$> feedbacks
      return Nothing
  where
    updateOrDeleteFeedback issues feedback =
      case L.find (\issue -> feedback ^. issueId == issue ^. issueId) issues of
        Just issue -> updateFeedbackById $ fromSimpleIssue feedback issue
        Nothing -> deleteFeedbackById (U.toString $ feedback ^. uuid)
