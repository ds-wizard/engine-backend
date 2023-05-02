module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper

instance ToSchema DocumentTemplateFormatDTO where
  declareNamedSchema = toSwagger (toFormatDTO formatJson)
