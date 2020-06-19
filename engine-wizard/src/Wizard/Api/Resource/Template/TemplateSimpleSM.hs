module Wizard.Api.Resource.Template.TemplateSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Util.Swagger
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleJM ()
import Wizard.Service.Template.TemplateMapper

instance ToSchema TemplateSimpleDTO where
  declareNamedSchema = simpleToSchema (toSimpleDTO commonWizardTemplate)
