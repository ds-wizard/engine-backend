module Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO

instance FromJSON SubmissionServiceSimpleDTO where
  parseJSON = simpleParseJSON "_submissionServiceSimpleDTO"

instance ToJSON SubmissionServiceSimpleDTO where
  toJSON = simpleToJSON "_submissionServiceSimpleDTO"
