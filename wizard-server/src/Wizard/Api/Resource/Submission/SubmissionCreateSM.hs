module Wizard.Api.Resource.Submission.SubmissionCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema SubmissionCreateDTO where
  declareNamedSchema = toSwagger submissionCreate
