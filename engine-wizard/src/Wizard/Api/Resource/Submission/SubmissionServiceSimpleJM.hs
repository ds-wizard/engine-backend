module Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO

instance FromJSON SubmissionServiceSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SubmissionServiceSimpleDTO where
  toJSON = genericToJSON simpleOptions
