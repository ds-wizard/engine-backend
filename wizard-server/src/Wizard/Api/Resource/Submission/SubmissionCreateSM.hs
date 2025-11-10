module Wizard.Api.Resource.Submission.SubmissionCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Database.Migration.Development.Submission.Data.Submissions

instance ToSchema SubmissionCreateDTO where
  declareNamedSchema = toSwagger submissionCreate
