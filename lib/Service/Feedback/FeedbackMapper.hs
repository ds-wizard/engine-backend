module Service.Feedback.FeedbackMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import LensesConfig
import Model.Feedback.Feedback

toDTO :: Feedback -> FeedbackDTO
toDTO feedback =
  FeedbackDTO
  { _feedbackDTOUuid = feedback ^. uuid
  , _feedbackDTOIssueId = feedback ^. issueId
  , _feedbackDTOQuestionUuid = feedback ^. questionUuid
  , _feedbackDTOPackageId = feedback ^. packageId
  , _feedbackDTOTitle = feedback ^. title
  , _feedbackDTOContent = feedback ^. content
  , _feedbackDTOCreatedAt = feedback ^. createdAt
  , _feedbackDTOUpdatedAt = feedback ^. updatedAt
  }

fromCreateDTO :: FeedbackCreateDTO -> U.UUID -> Int -> UTCTime -> Feedback
fromCreateDTO dto fUuid issueId now =
  Feedback
  { _feedbackUuid = fUuid
  , _feedbackIssueId = issueId
  , _feedbackQuestionUuid = dto ^. questionUuid
  , _feedbackPackageId = dto ^. packageId
  , _feedbackTitle = dto ^. title
  , _feedbackContent = dto ^. content
  , _feedbackCreatedAt = now
  , _feedbackUpdatedAt = now
  }
