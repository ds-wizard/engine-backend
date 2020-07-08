module Wizard.Api.Resource.Template.TemplateSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleJM ()
import Wizard.Api.Resource.Template.TemplateStateSM ()
import Wizard.Database.Migration.Development.Template.Data.Templates

instance ToSchema TemplateSimpleDTO where
  declareNamedSchema = simpleToSchema commonWizardTemplateSimpleDTO
