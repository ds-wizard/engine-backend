module Wizard.Api.Resource.Submission.SubmissionCreateSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Database.Migration.Development.Submission.Data.Submissions

instance ToSchema SubmissionCreateDTO where
  declareNamedSchema = simpleToSchema submissionCreate
