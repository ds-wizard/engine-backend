module Wizard.Service.Feedback.FeedbackMapper where

import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Feedback.Feedback
import Wizard.Service.Feedback.FeedbackUtil

toDTO :: ServerConfig -> AppConfig -> Feedback -> FeedbackDTO
toDTO serverConfig appConfig f =
  FeedbackDTO
    { uuid = f.uuid
    , issueId = f.issueId
    , issueUrl = createIssueUrl serverConfig.feedback appConfig.questionnaire.feedback f
    , questionUuid = f.questionUuid
    , packageId = f.packageId
    , title = f.title
    , content = f.content
    , createdAt = f.createdAt
    , updatedAt = f.updatedAt
    }

fromCreateDTO :: FeedbackCreateDTO -> U.UUID -> Int -> U.UUID -> UTCTime -> Feedback
fromCreateDTO dto fUuid issueId appUuid now =
  Feedback
    { uuid = fUuid
    , issueId = issueId
    , questionUuid = dto.questionUuid
    , packageId = dto.packageId
    , title = dto.title
    , content = dto.content
    , appUuid = appUuid
    , createdAt = now
    , updatedAt = now
    }

fromSimpleIssue :: Feedback -> IssueIDTO -> UTCTime -> Feedback
fromSimpleIssue feedback simpleIssue now =
  Feedback
    { uuid = feedback.uuid
    , issueId = simpleIssue.number
    , questionUuid = feedback.questionUuid
    , packageId = feedback.packageId
    , title = simpleIssue.title
    , content = simpleIssue.body
    , appUuid = feedback.appUuid
    , createdAt = feedback.createdAt
    , updatedAt = now
    }
