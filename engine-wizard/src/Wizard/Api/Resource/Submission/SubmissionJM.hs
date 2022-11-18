module Wizard.Api.Resource.Submission.SubmissionJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Submission.Submission

instance FromJSON SubmissionState

instance ToJSON SubmissionState

instance FromJSON SubmissionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionDTO where
  toJSON = genericToJSON jsonOptions
