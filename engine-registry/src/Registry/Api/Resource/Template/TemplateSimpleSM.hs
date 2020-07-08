module Registry.Api.Resource.Template.TemplateSimpleSM where

import Data.Swagger

import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Api.Resource.Template.TemplateSimpleJM ()
import Registry.Database.Migration.Development.Template.Data.Templates
import Shared.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Util.Swagger

instance ToSchema TemplateSimpleDTO where
  declareNamedSchema = simpleToSchema commonWizardTemplateSimpleDTO
