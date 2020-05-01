module Wizard.Api.Resource.Template.TemplateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates

instance ToSchema TemplateDTO where
  declareNamedSchema = simpleToSchema commonWizardTemplate

instance ToSchema TemplateAllowedKMDTO where
  declareNamedSchema = simpleToSchema templateAllowedKM

instance ToSchema TemplateFormatDTO where
  declareNamedSchema = simpleToSchema templateFormatJson
