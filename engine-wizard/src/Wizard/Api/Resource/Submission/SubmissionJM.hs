module Wizard.Api.Resource.Submission.SubmissionJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Submission.SubmissionDTO

instance FromJSON SubmissionDTO where
  parseJSON = simpleParseJSON "_submissionDTO"

instance ToJSON SubmissionDTO where
  toJSON = simpleToJSON "_submissionDTO"
