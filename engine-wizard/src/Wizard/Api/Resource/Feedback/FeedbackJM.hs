module Wizard.Api.Resource.Feedback.FeedbackJM where

import Data.Aeson

import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON FeedbackDTO where
  parseJSON = simpleParseJSON "_feedbackDTO"

instance ToJSON FeedbackDTO where
  toJSON = simpleToJSON "_feedbackDTO"
