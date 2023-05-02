module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()

instance ToSchema DocumentTemplateChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDeprecatedChangeDTO
