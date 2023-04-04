module Wizard.Service.Feedback.FeedbackService (
  getFeedbacksFiltered,
  createFeedback,
  createFeedbackWithGivenUuid,
  getFeedbackByUuid,
  synchronizeFeedbacksInAllApplications,
) where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Integration.Http.GitHub.Runner
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextResult
import Wizard.Model.Feedback.Feedback
import Wizard.Service.Common
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Context.ContextService
import Wizard.Service.Feedback.FeedbackMapper
import Wizard.Util.Logger

getFeedbacksFiltered :: [(String, String)] -> AppContextM [FeedbackDTO]
getFeedbacksFiltered queryParams = do
  checkIfFeedbackIsEnabled
  feedbacks <- findFeedbacksFiltered queryParams
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  return . fmap (toDTO serverConfig appConfig) $ feedbacks

createFeedback :: FeedbackCreateDTO -> AppContextM FeedbackDTO
createFeedback reqDto =
  runInTransaction $ do
    checkIfFeedbackIsEnabled
    fUuid <- liftIO generateUuid
    createFeedbackWithGivenUuid fUuid reqDto

createFeedbackWithGivenUuid :: U.UUID -> FeedbackCreateDTO -> AppContextM FeedbackDTO
createFeedbackWithGivenUuid fUuid reqDto =
  runInTransaction $ do
    checkIfFeedbackIsEnabled
    issue <- createIssue reqDto.packageId reqDto.questionUuid reqDto.title reqDto.content
    now <- liftIO getCurrentTime
    appUuid <- asks currentAppUuid
    let feedback = fromCreateDTO reqDto fUuid issue.number appUuid now
    insertFeedback feedback
    serverConfig <- asks serverConfig
    appConfig <- getAppConfig
    return $ toDTO serverConfig appConfig feedback

getFeedbackByUuid :: U.UUID -> AppContextM FeedbackDTO
getFeedbackByUuid fUuid = do
  checkIfFeedbackIsEnabled
  feedback <- findFeedbackByUuid fUuid
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  return $ toDTO serverConfig appConfig feedback

synchronizeFeedbacksInAllApplications :: AppContextM ()
synchronizeFeedbacksInAllApplications = runFunctionForAllApps "synchronizeFeedbacks" synchronizeFeedbacks

-- --------------------------------
-- PRIVATE
-- --------------------------------
synchronizeFeedbacks :: AppContextM (ContextResult, Maybe String)
synchronizeFeedbacks =
  catchError
    ( do
        runInTransaction $ do
          appConfig <- getAppConfig
          if appConfig.questionnaire.feedback.enabled
            then do
              logInfoU _CMP_WORKER "synchronizing feedback"
              issues <- getIssues
              feedbacks <- findFeedbacks
              now <- liftIO getCurrentTime
              traverse_ (updateOrDeleteFeedback issues now) feedbacks
            else logInfoU _CMP_WORKER "synchronization is disabled"
          return (SuccessContextResult, Nothing)
    )
    ( \error -> do
        if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 401")
          then return (SuccessContextResult, Just "Wrong GitHub token")
          else return (ErrorContextResult, Just . show $ error)
    )
  where
    updateOrDeleteFeedback issues now feedback =
      case L.find (\issue -> feedback.issueId == issue.number) issues of
        Just issue -> updateFeedbackByUuid $ fromSimpleIssue feedback issue now
        Nothing -> deleteFeedbackByUuid feedback.uuid

checkIfFeedbackIsEnabled = checkIfAppFeatureIsEnabled "Feedback" (\c -> c.questionnaire.feedback.enabled)
