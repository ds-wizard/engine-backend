module Wizard.Api.Resource.Feedback.FeedbackJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Feedback.FeedbackDTO

instance FromJSON FeedbackDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON FeedbackDTO where
  toJSON = genericToJSON simpleOptions
