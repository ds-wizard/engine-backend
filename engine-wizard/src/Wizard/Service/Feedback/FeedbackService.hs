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
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Integration.Http.GitHub.Runner
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Feedback.Feedback
import Wizard.Service.Common
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Feedback.FeedbackMapper
import Wizard.Util.Interpolation (interpolateString)

getFeedbacksFiltered :: [(String, String)] -> AppContextM [FeedbackDTO]
getFeedbacksFiltered queryParams = do
  checkIfFeedbackIsEnabled
  fbks <- findFeedbacksFiltered queryParams
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  return $ (\f -> toDTO f (createIssueUrl (serverConfig ^. feedback) (appConfig ^. questionnaire . feedback) f)) <$>
    fbks

createFeedback :: FeedbackCreateDTO -> AppContextM FeedbackDTO
createFeedback reqDto = do
  checkIfFeedbackIsEnabled
  fUuid <- liftIO generateUuid
  createFeedbackWithGivenUuid fUuid reqDto

createFeedbackWithGivenUuid :: U.UUID -> FeedbackCreateDTO -> AppContextM FeedbackDTO
createFeedbackWithGivenUuid fUuid reqDto = do
  checkIfFeedbackIsEnabled
  issue <- createIssue (reqDto ^. packageId) (reqDto ^. questionUuid) (reqDto ^. title) (reqDto ^. content)
  now <- liftIO getCurrentTime
  let fbk = fromCreateDTO reqDto fUuid (issue ^. number) now
  insertFeedback fbk
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  let iUrl = createIssueUrl (serverConfig ^. feedback) (appConfig ^. questionnaire . feedback) fbk
  return $ toDTO fbk iUrl

getFeedbackByUuid :: String -> AppContextM FeedbackDTO
getFeedbackByUuid fUuid = do
  checkIfFeedbackIsEnabled
  fbk <- findFeedbackById fUuid
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  let iUrl = createIssueUrl (serverConfig ^. feedback) (appConfig ^. questionnaire . feedback) fbk
  return $ toDTO fbk iUrl

synchronizeFeedbacks :: AppContextM ()
synchronizeFeedbacks = do
  checkIfFeedbackIsEnabled
  issues <- getIssues
  fbks <- findFeedbacks
  now <- liftIO getCurrentTime
  sequence_ $ updateOrDeleteFeedback issues now <$> fbks
  where
    updateOrDeleteFeedback issues now fbk =
      case L.find (\issue -> fbk ^. issueId == issue ^. number) issues of
        Just issue -> updateFeedbackById $ fromSimpleIssue fbk issue now
        Nothing -> deleteFeedbackById (U.toString $ fbk ^. uuid)

createIssueUrl :: ServerConfigFeedback -> AppConfigQuestionnaireFeedback -> Feedback -> String
createIssueUrl serverConfig appConfig fbk =
  let urlTemplate = (serverConfig ^. webUrl) ++ "/${owner}/${repo}/issues/${issueId}"
      variables =
        M.fromList [("owner", appConfig ^. owner), ("repo", appConfig ^. repo), ("issueId", show $ fbk ^. issueId)]
   in interpolateString variables urlTemplate

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfFeedbackIsEnabled = checkIfAppFeatureIsEnabled "Feedback" (questionnaire . feedback . enabled)
