module Wizard.Service.Feedback.FeedbackService
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
import Data.Text (Text)
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Error.Error
import Shared.Util.String
import Shared.Util.Uuid
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.LensesConfig
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Feedback.Feedback
import Wizard.Service.Common
import Wizard.Service.Feedback.Connector.Connector
import Wizard.Service.Feedback.Connector.GitHub.GitHubConnector ()
import Wizard.Service.Feedback.FeedbackMapper

getFeedbacksFiltered :: [(Text, Text)] -> AppContextM (Either AppError [FeedbackDTO])
getFeedbacksFiltered queryParams =
  heCheckIfFeedbackIsEnabled $ heFindFeedbacksFiltered queryParams $ \feedbacks -> do
    appConfig <- asks _appContextApplicationConfig
    return . Right $ (\f -> toDTO f (createIssueUrl appConfig f)) <$> feedbacks

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
    appConfig <- asks _appContextApplicationConfig
    let iUrl = createIssueUrl appConfig feedback
    return . Right $ toDTO feedback iUrl

getFeedbackByUuid :: String -> AppContextM (Either AppError FeedbackDTO)
getFeedbackByUuid fUuid =
  heCheckIfFeedbackIsEnabled $ heFindFeedbackById fUuid $ \feedback -> do
    appConfig <- asks _appContextApplicationConfig
    let iUrl = createIssueUrl appConfig feedback
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
createIssueUrl appConfig f =
  let fIssueUrlTemplate = appConfig ^. feedback . issueUrl
      fOwner = appConfig ^. feedback . owner
      fRepo = appConfig ^. feedback . repo
      fIssueId = show $ f ^. issueId
   in replace ":owner" fOwner . replace ":repo" fRepo . replace ":issueId" fIssueId $ fIssueUrlTemplate

-- --------------------------------
-- PRIVATE
-- --------------------------------
heCheckIfFeedbackIsEnabled = heCheckIfFeatureIsEnabled "Feedback" (feedback . enabled)

hmCheckIfFeedbackIsEnabled = hmCheckIfFeatureIsEnabled "Feedback" (feedback . enabled)
