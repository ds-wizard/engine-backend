module Wizard.Api.Resource.Submission.SubmissionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Model.Submission.Submission
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON SubmissionState

instance ToJSON SubmissionState

instance FromJSON SubmissionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionDTO where
  toJSON = genericToJSON jsonOptions
