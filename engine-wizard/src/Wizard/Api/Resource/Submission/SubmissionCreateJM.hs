module Wizard.Api.Resource.Submission.SubmissionCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Submission.SubmissionCreateDTO

instance FromJSON SubmissionCreateDTO where
  parseJSON = simpleParseJSON "_submissionCreateDTO"

instance ToJSON SubmissionCreateDTO where
  toJSON = simpleToJSON "_submissionCreateDTO"
