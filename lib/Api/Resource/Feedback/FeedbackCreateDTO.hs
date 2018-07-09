module Api.Resource.Feedback.FeedbackCreateDTO where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U

data FeedbackCreateDTO = FeedbackCreateDTO
  { _feedbackCreateDTOQuestionUuid :: U.UUID
  , _feedbackCreateDTOPackageId :: String
  , _feedbackCreateDTOTitle :: String
  , _feedbackCreateDTOContent :: String
  } deriving (Show)

instance Eq FeedbackCreateDTO where
  a == b =
    _feedbackCreateDTOQuestionUuid a == _feedbackCreateDTOQuestionUuid b &&
    _feedbackCreateDTOPackageId a == _feedbackCreateDTOPackageId b &&
    _feedbackCreateDTOTitle a == _feedbackCreateDTOTitle b && _feedbackCreateDTOContent a == _feedbackCreateDTOContent b

instance FromJSON FeedbackCreateDTO where
  parseJSON (Object o) = do
    _feedbackCreateDTOQuestionUuid <- o .: "questionUuid"
    _feedbackCreateDTOPackageId <- o .: "packageId"
    _feedbackCreateDTOTitle <- o .: "title"
    _feedbackCreateDTOContent <- o .: "content"
    return FeedbackCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON FeedbackCreateDTO where
  toJSON FeedbackCreateDTO {..} =
    object
      [ "questionUuid" .= _feedbackCreateDTOQuestionUuid
      , "packageId" .= _feedbackCreateDTOPackageId
      , "title" .= _feedbackCreateDTOTitle
      , "content" .= _feedbackCreateDTOContent
      ]
