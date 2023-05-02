module Wizard.Api.Resource.Feedback.FeedbackJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Feedback.FeedbackDTO

instance FromJSON FeedbackDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FeedbackDTO where
  toJSON = genericToJSON jsonOptions
