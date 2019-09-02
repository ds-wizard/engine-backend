module Api.Resource.Feedback.FeedbackDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data FeedbackDTO = FeedbackDTO
  { _feedbackDTOUuid :: U.UUID
  , _feedbackDTOIssueId :: Int
  , _feedbackDTOIssueUrl :: String
  , _feedbackDTOQuestionUuid :: U.UUID
  , _feedbackDTOPackageId :: String
  , _feedbackDTOTitle :: String
  , _feedbackDTOContent :: String
  , _feedbackDTOCreatedAt :: UTCTime
  , _feedbackDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq FeedbackDTO where
  a == b =
    _feedbackDTOUuid a == _feedbackDTOUuid b &&
    _feedbackDTOIssueId a == _feedbackDTOIssueId b &&
    _feedbackDTOIssueUrl a == _feedbackDTOIssueUrl b &&
    _feedbackDTOQuestionUuid a == _feedbackDTOQuestionUuid b &&
    _feedbackDTOPackageId a == _feedbackDTOPackageId b &&
    _feedbackDTOTitle a == _feedbackDTOTitle b && _feedbackDTOContent a == _feedbackDTOContent b
