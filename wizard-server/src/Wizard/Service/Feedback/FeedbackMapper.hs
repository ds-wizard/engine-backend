module Wizard.Service.Feedback.FeedbackMapper where

import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Feedback.FeedbackUtil

toDTO :: ServerConfig -> TenantConfigProject -> Feedback -> FeedbackDTO
toDTO serverConfig tcProject f =
  FeedbackDTO
    { uuid = f.uuid
    , issueId = f.issueId
    , issueUrl = createIssueUrl serverConfig.feedback tcProject.feedback f
    , questionUuid = f.questionUuid
    , knowledgeModelPackageId = f.knowledgeModelPackageId
    , title = f.title
    , content = f.content
    , createdAt = f.createdAt
    , updatedAt = f.updatedAt
    }

fromCreateDTO :: FeedbackCreateDTO -> U.UUID -> Int -> U.UUID -> UTCTime -> Feedback
fromCreateDTO dto fUuid issueId tenantUuid now =
  Feedback
    { uuid = fUuid
    , issueId = issueId
    , questionUuid = dto.questionUuid
    , knowledgeModelPackageId = dto.knowledgeModelPackageId
    , title = dto.title
    , content = dto.content
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromSimpleIssue :: Feedback -> IssueIDTO -> UTCTime -> Feedback
fromSimpleIssue feedback simpleIssue now =
  Feedback
    { uuid = feedback.uuid
    , issueId = simpleIssue.number
    , questionUuid = feedback.questionUuid
    , knowledgeModelPackageId = feedback.knowledgeModelPackageId
    , title = simpleIssue.title
    , content = simpleIssue.body
    , tenantUuid = feedback.tenantUuid
    , createdAt = feedback.createdAt
    , updatedAt = now
    }
