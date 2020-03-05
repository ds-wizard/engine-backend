module Wizard.Api.Resource.Feedback.FeedbackJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Feedback.FeedbackDTO

instance FromJSON FeedbackDTO where
  parseJSON = simpleParseJSON "_feedbackDTO"

instance ToJSON FeedbackDTO where
  toJSON = simpleToJSON "_feedbackDTO"
