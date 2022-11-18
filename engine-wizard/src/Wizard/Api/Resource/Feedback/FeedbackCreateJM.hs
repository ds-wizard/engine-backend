module Wizard.Api.Resource.Feedback.FeedbackCreateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO

instance FromJSON FeedbackCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FeedbackCreateDTO where
  toJSON = genericToJSON jsonOptions
