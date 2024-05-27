module Wizard.Api.Resource.Submission.SubmissionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import Wizard.Model.Submission.Submission
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema SubmissionState

instance ToSchema SubmissionDTO where
  declareNamedSchema = toSwagger submission1Dto
