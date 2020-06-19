module Wizard.Api.Resource.Template.TemplateSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Template.TemplateDTO

instance ToSchema TemplateDTO where
  declareNamedSchema = simpleToSchema commonWizardTemplate
