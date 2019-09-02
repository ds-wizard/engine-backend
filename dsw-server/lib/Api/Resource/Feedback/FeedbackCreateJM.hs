module Api.Resource.Feedback.FeedbackCreateJM where

import Data.Aeson

import Api.Resource.Feedback.FeedbackCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON FeedbackCreateDTO where
  parseJSON = simpleParseJSON "_feedbackCreateDTO"

instance ToJSON FeedbackCreateDTO where
  toJSON = simpleToJSON "_feedbackCreateDTO"
