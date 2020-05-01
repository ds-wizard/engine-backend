module Wizard.Api.Resource.Submission.SubmissionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Database.Migration.Development.Submission.Data.Submissions

instance ToSchema SubmissionDTO where
  declareNamedSchema = simpleToSchema submission1
