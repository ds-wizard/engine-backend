module Wizard.Service.Feedback.FeedbackMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import LensesConfig
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
    { _feedbackDTOUuid = f ^. uuid
    , _feedbackDTOIssueId = f ^. issueId
    , _feedbackDTOIssueUrl = createIssueUrl (serverConfig ^. feedback) (appConfig ^. questionnaire . feedback) f
    , _feedbackDTOQuestionUuid = f ^. questionUuid
    , _feedbackDTOPackageId = f ^. packageId
    , _feedbackDTOTitle = f ^. title
    , _feedbackDTOContent = f ^. content
    , _feedbackDTOCreatedAt = f ^. createdAt
    , _feedbackDTOUpdatedAt = f ^. updatedAt
    }

fromCreateDTO :: FeedbackCreateDTO -> U.UUID -> Int -> U.UUID -> UTCTime -> Feedback
fromCreateDTO dto fUuid issueId appUuid now =
  Feedback
    { _feedbackUuid = fUuid
    , _feedbackIssueId = issueId
    , _feedbackQuestionUuid = dto ^. questionUuid
    , _feedbackPackageId = dto ^. packageId
    , _feedbackTitle = dto ^. title
    , _feedbackContent = dto ^. content
    , _feedbackAppUuid = appUuid
    , _feedbackCreatedAt = now
    , _feedbackUpdatedAt = now
    }

fromSimpleIssue :: Feedback -> IssueIDTO -> UTCTime -> Feedback
fromSimpleIssue feedback simpleIssue now =
  Feedback
    { _feedbackUuid = feedback ^. uuid
    , _feedbackIssueId = simpleIssue ^. number
    , _feedbackQuestionUuid = feedback ^. questionUuid
    , _feedbackPackageId = feedback ^. packageId
    , _feedbackTitle = simpleIssue ^. title
    , _feedbackContent = simpleIssue ^. body
    , _feedbackAppUuid = feedback ^. appUuid
    , _feedbackCreatedAt = feedback ^. createdAt
    , _feedbackUpdatedAt = now
    }
