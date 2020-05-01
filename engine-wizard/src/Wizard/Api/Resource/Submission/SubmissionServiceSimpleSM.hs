module Wizard.Api.Resource.Submission.SubmissionServiceSimpleSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Service.Submission.SubmissionMapper

instance ToSchema SubmissionServiceSimpleDTO where
  declareNamedSchema = simpleToSchema (toSubmissionServiceSimpleDTO defaultSubmissionService)
