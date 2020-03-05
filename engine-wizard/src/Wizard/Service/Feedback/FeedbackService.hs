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
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.String
import Shared.Util.Uuid
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Feedback.Feedback
import Wizard.Service.Common
import Wizard.Service.Feedback.Connector.Connector
import Wizard.Service.Feedback.Connector.GitHub.GitHubConnector ()
import Wizard.Service.Feedback.FeedbackMapper

getFeedbacksFiltered :: [(String, String)] -> AppContextM [FeedbackDTO]
getFeedbacksFiltered queryParams = do
  checkIfFeedbackIsEnabled
  feedbacks <- findFeedbacksFiltered queryParams
  appConfig <- asks _appContextApplicationConfig
  return $ (\f -> toDTO f (createIssueUrl appConfig f)) <$> feedbacks

createFeedback :: FeedbackCreateDTO -> AppContextM FeedbackDTO
createFeedback reqDto = do
  checkIfFeedbackIsEnabled
  fUuid <- liftIO generateUuid
  createFeedbackWithGivenUuid fUuid reqDto

createFeedbackWithGivenUuid :: U.UUID -> FeedbackCreateDTO -> AppContextM FeedbackDTO
createFeedbackWithGivenUuid fUuid reqDto = do
  checkIfFeedbackIsEnabled
  issueId <- createIssue (reqDto ^. packageId) (reqDto ^. questionUuid) (reqDto ^. title) (reqDto ^. content)
  now <- liftIO getCurrentTime
  let feedback = fromCreateDTO reqDto fUuid issueId now
  insertFeedback feedback
  appConfig <- asks _appContextApplicationConfig
  let iUrl = createIssueUrl appConfig feedback
  return $ toDTO feedback iUrl

getFeedbackByUuid :: String -> AppContextM FeedbackDTO
getFeedbackByUuid fUuid = do
  checkIfFeedbackIsEnabled
  feedback <- findFeedbackById fUuid
  appConfig <- asks _appContextApplicationConfig
  let iUrl = createIssueUrl appConfig feedback
  return $ toDTO feedback iUrl

synchronizeFeedbacks :: AppContextM ()
synchronizeFeedbacks = do
  checkIfFeedbackIsEnabled
  issues <- getIssues
  feedbacks <- findFeedbacks
  now <- liftIO getCurrentTime
  sequence $ (updateOrDeleteFeedback issues now) <$> feedbacks
  return ()
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
checkIfFeedbackIsEnabled = checkIfFeatureIsEnabled "Feedback" (feedback . enabled)
