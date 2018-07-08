module Api.Resource.Feedback.FeedbackDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import qualified Data.UUID as U

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
  } deriving (Show)

instance Eq FeedbackDTO where
  a == b =
    _feedbackDTOUuid a == _feedbackDTOUuid b &&
    _feedbackDTOIssueId a == _feedbackDTOIssueId b &&
    _feedbackDTOIssueUrl a == _feedbackDTOIssueUrl b &&
    _feedbackDTOQuestionUuid a == _feedbackDTOQuestionUuid b &&
    _feedbackDTOPackageId a == _feedbackDTOPackageId b &&
    _feedbackDTOTitle a == _feedbackDTOTitle b && _feedbackDTOContent a == _feedbackDTOContent b

instance FromJSON FeedbackDTO where
  parseJSON (Object o) = do
    _feedbackDTOUuid <- o .: "uuid"
    _feedbackDTOIssueId <- o .: "issueId"
    _feedbackDTOIssueUrl <- o .: "issueUrl"
    _feedbackDTOQuestionUuid <- o .: "questionUuid"
    _feedbackDTOPackageId <- o .: "packageId"
    _feedbackDTOTitle <- o .: "title"
    _feedbackDTOContent <- o .: "content"
    _feedbackDTOCreatedAt <- o .: "createdAt"
    _feedbackDTOUpdatedAt <- o .: "updatedAt"
    return FeedbackDTO {..}
  parseJSON _ = mzero

instance ToJSON FeedbackDTO where
  toJSON FeedbackDTO {..} =
    object
      [ "uuid" .= _feedbackDTOUuid
      , "issueId" .= _feedbackDTOIssueId
      , "issueUrl" .= _feedbackDTOIssueUrl
      , "questionUuid" .= _feedbackDTOQuestionUuid
      , "packageId" .= _feedbackDTOPackageId
      , "title" .= _feedbackDTOTitle
      , "content" .= _feedbackDTOContent
      , "createdAt" .= _feedbackDTOCreatedAt
      , "updatedAt" .= _feedbackDTOUpdatedAt
      ]
