module Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO

instance FromJSON SubmissionServiceSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionServiceSimpleDTO where
  toJSON = genericToJSON jsonOptions
