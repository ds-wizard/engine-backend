module Wizard.Api.Resource.Feedback.FeedbackCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO

instance FromJSON FeedbackCreateDTO where
  parseJSON = simpleParseJSON "_feedbackCreateDTO"

instance ToJSON FeedbackCreateDTO where
  toJSON = simpleToJSON "_feedbackCreateDTO"
