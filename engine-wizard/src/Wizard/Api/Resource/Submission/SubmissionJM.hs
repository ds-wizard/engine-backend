module Wizard.Api.Resource.Submission.SubmissionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Submission.SubmissionDTO

instance FromJSON SubmissionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SubmissionDTO where
  toJSON = genericToJSON simpleOptions
