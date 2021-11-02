module Wizard.Service.Feedback.FeedbackMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import LensesConfig
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Feedback.Feedback

toDTO :: Feedback -> String -> FeedbackDTO
toDTO feedback issueUrl =
  FeedbackDTO
    { _feedbackDTOUuid = feedback ^. uuid
    , _feedbackDTOIssueId = feedback ^. issueId
    , _feedbackDTOIssueUrl = issueUrl
    , _feedbackDTOQuestionUuid = feedback ^. questionUuid
    , _feedbackDTOPackageId = feedback ^. packageId
    , _feedbackDTOTitle = feedback ^. title
    , _feedbackDTOContent = feedback ^. content
    , _feedbackDTOCreatedAt = feedback ^. createdAt
    , _feedbackDTOUpdatedAt = feedback ^. updatedAt
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
