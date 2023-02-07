module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

instance ToSchema DocumentTemplateChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDeprecatedChangeDTO
