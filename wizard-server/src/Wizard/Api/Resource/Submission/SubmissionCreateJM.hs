module Wizard.Api.Resource.Submission.SubmissionCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Submission.SubmissionCreateDTO

instance FromJSON SubmissionCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionCreateDTO where
  toJSON = genericToJSON jsonOptions
