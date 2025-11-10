module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

instance ToSchema DocumentTemplateChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDeprecatedChangeDTO
