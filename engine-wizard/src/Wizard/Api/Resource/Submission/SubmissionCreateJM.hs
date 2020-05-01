module Wizard.Api.Resource.Submission.SubmissionCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Submission.SubmissionCreateDTO

instance FromJSON SubmissionCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SubmissionCreateDTO where
  toJSON = genericToJSON simpleOptions
