module Wizard.Api.Resource.Document.DocumentSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Wizard.Api.Resource.Project.ProjectSimpleSM ()
import Wizard.Api.Resource.Submission.SubmissionSM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Model.Document.Document

instance ToSchema DocumentState

instance ToSchema DocumentDTO where
  declareNamedSchema = toSwagger doc1Dto
