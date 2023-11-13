module Wizard.Api.Resource.Submission.SubmissionServiceSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Service.Submission.SubmissionMapper

instance ToSchema SubmissionServiceSimpleDTO where
  declareNamedSchema = toSwagger (toSubmissionServiceSimpleDTO defaultSubmissionService)
