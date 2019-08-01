module Api.Resource.Feedback.FeedbackJM where

import Data.Aeson

import Api.Resource.Feedback.FeedbackDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON FeedbackDTO where
  parseJSON = simpleParseJSON "_feedbackDTO"

instance ToJSON FeedbackDTO where
  toJSON = simpleToJSON "_feedbackDTO"
