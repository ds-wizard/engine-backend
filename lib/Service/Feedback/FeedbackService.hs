module Service.Feedback.FeedbackService
  ( getFeedbacksFiltered
  , createFeedback
  , createFeedbackWithGivenUuid
  , getFeedbackByUuid
  , synchronizeFeedbacks
  , createIssueUrl
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import Data.List.Utils as DLU
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import Database.DAO.Feedback.FeedbackDAO
import LensesConfig
import Model.Config.AppConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Feedback.Feedback
import Service.Common
import Service.Feedback.Connector.Connector
import Service.Feedback.Connector.GitHub.GitHubConnector ()
import Service.Feedback.FeedbackMapper
import Util.Uuid

getFeedbacksFiltered :: [(Text, Text)] -> AppContextM (Either AppError [FeedbackDTO])
getFeedbacksFiltered queryParams =
  heCheckIfFeedbackIsEnabled $ heFindFeedbacksFiltered queryParams $ \feedbacks -> do
    dswConfig <- asks _appContextAppConfig
    return . Right $ (\f -> toDTO f (createIssueUrl dswConfig f)) <$> feedbacks

createFeedback :: FeedbackCreateDTO -> AppContextM (Either AppError FeedbackDTO)
createFeedback reqDto =
  heCheckIfFeedbackIsEnabled $ do
    fUuid <- liftIO generateUuid
    createFeedbackWithGivenUuid fUuid reqDto

createFeedbackWithGivenUuid :: U.UUID -> FeedbackCreateDTO -> AppContextM (Either AppError FeedbackDTO)
createFeedbackWithGivenUuid fUuid reqDto =
  heCheckIfFeedbackIsEnabled $
  heCreateIssue (reqDto ^. packageId) (reqDto ^. questionUuid) (reqDto ^. title) (reqDto ^. content) $ \issueId -> do
    now <- liftIO getCurrentTime
    let feedback = fromCreateDTO reqDto fUuid issueId now
    insertFeedback feedback
    dswConfig <- asks _appContextAppConfig
    let iUrl = createIssueUrl dswConfig feedback
    return . Right $ toDTO feedback iUrl

getFeedbackByUuid :: String -> AppContextM (Either AppError FeedbackDTO)
getFeedbackByUuid fUuid =
  heCheckIfFeedbackIsEnabled $ heFindFeedbackById fUuid $ \feedback -> do
    dswConfig <- asks _appContextAppConfig
    let iUrl = createIssueUrl dswConfig feedback
    return . Right $ toDTO feedback iUrl

synchronizeFeedbacks :: AppContextM (Maybe AppError)
synchronizeFeedbacks =
  hmCheckIfFeedbackIsEnabled $ hmGetIssues $ \issues ->
    hmFindFeedbacks $ \feedbacks -> do
      now <- liftIO getCurrentTime
      sequence $ (updateOrDeleteFeedback issues now) <$> feedbacks
      return Nothing
  where
    updateOrDeleteFeedback issues now feedback =
      case L.find (\issue -> feedback ^. issueId == issue ^. issueId) issues of
        Just issue -> updateFeedbackById $ fromSimpleIssue feedback issue now
        Nothing -> deleteFeedbackById (U.toString $ feedback ^. uuid)

createIssueUrl :: AppConfig -> Feedback -> String
createIssueUrl dswConfig f =
  let fIssueUrlTemplate = fromMaybe "" $ dswConfig ^. feedback . issueUrl
      fOwner = fromMaybe "" $ dswConfig ^. feedback . owner
      fRepo = fromMaybe "" $ dswConfig ^. feedback . repo
      fIssueId = show $ f ^. issueId
  in replace ":owner" fOwner . replace ":repo" fRepo . replace ":issueId" fIssueId $ fIssueUrlTemplate

-- --------------------------------
-- PRIVATE
-- --------------------------------
heCheckIfFeedbackIsEnabled = heCheckIfFeatureIsEnabled "Feedback" (feedback . enabled)

hmCheckIfFeedbackIsEnabled = hmCheckIfFeatureIsEnabled "Feedback" (feedback . enabled)
