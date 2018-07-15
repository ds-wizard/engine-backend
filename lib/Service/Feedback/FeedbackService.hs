module Service.Feedback.FeedbackService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import Data.List.Utils as DLU
import Data.Text (Text)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import Database.DAO.Feedback.FeedbackDAO
import LensesConfig
import Model.Config.DSWConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Feedback.Feedback
import Service.Feedback.Connector.Connector
import Service.Feedback.Connector.GitHub.GitHubConnector ()
import Service.Feedback.FeedbackMapper
import Util.Uuid

getFeedbacksFiltered :: [(Text, Text)] -> AppContextM (Either AppError [FeedbackDTO])
getFeedbacksFiltered queryParams =
  heFindFeedbacksFiltered queryParams $ \feedbacks -> do
    dswConfig <- asks _appContextConfig
    return . Right $ (\f -> toDTO f (createIssueUrl dswConfig f)) <$> feedbacks

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
    dswConfig <- asks _appContextConfig
    let iUrl = createIssueUrl dswConfig feedback
    return . Right $ toDTO feedback iUrl

getFeedbackByUuid :: String -> AppContextM (Either AppError FeedbackDTO)
getFeedbackByUuid fUuid =
  heFindFeedbackById fUuid $ \feedback -> do
    dswConfig <- asks _appContextConfig
    let iUrl = createIssueUrl dswConfig feedback
    return . Right $ toDTO feedback iUrl

synchronizeFeedbacks :: AppContextM (Maybe AppError)
synchronizeFeedbacks =
  hmGetIssues $ \issues ->
    hmFindFeedbacks $ \feedbacks -> do
      now <- liftIO getCurrentTime
      sequence $ (updateOrDeleteFeedback issues now) <$> feedbacks
      return Nothing
  where
    updateOrDeleteFeedback issues now feedback =
      case L.find (\issue -> feedback ^. issueId == issue ^. issueId) issues of
        Just issue -> updateFeedbackById $ fromSimpleIssue feedback issue now
        Nothing -> deleteFeedbackById (U.toString $ feedback ^. uuid)

createIssueUrl :: DSWConfig -> Feedback -> String
createIssueUrl dswConfig f =
  let fIssueUrlTemplate = dswConfig ^. feedback . issueUrl
      fOwner = dswConfig ^. feedback . owner
      fRepo = dswConfig ^. feedback . repo
      fIssueId = show $ f ^. issueId
  in replace ":owner" fOwner . replace ":dsw-staging" fRepo . replace ":issueId" fIssueId $ fIssueUrlTemplate
