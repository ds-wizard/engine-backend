module Api.Resource.Feedback.FeedbackCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data FeedbackCreateDTO = FeedbackCreateDTO
  { _feedbackCreateDTOQuestionUuid :: U.UUID
  , _feedbackCreateDTOPackageId :: String
  , _feedbackCreateDTOTitle :: String
  , _feedbackCreateDTOContent :: String
  } deriving (Show, Eq, Generic)
