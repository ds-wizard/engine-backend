module Wizard.Api.Resource.Feedback.FeedbackCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO

instance FromJSON FeedbackCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON FeedbackCreateDTO where
  toJSON = genericToJSON simpleOptions
