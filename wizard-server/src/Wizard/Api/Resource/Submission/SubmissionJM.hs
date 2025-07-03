module Wizard.Api.Resource.Submission.SubmissionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Submission.Submission
import Wizard.Model.Submission.SubmissionList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON SubmissionState

instance ToJSON SubmissionState

instance FromJSON SubmissionList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionList where
  toJSON = genericToJSON jsonOptions
