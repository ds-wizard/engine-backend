module Registry.Api.Resource.Template.TemplateDetailSM where

import Data.Swagger

import Registry.Api.Resource.Package.PackageSimpleSM ()
import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateDetailJM ()
import Registry.Database.Migration.Development.Template.Data.Templates
import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Util.Swagger

instance ToSchema TemplateDetailDTO where
  declareNamedSchema = toSwagger commonWizardTemplateDetailDTO
