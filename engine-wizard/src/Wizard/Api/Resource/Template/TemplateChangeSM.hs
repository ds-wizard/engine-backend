module Wizard.Api.Resource.Template.TemplateChangeSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates

instance ToSchema TemplateChangeDTO where
  declareNamedSchema = toSwagger commonWizardTemplateEditedChangeDto
