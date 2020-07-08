module Wizard.Api.Resource.Template.TemplateDetailSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateDetailJM ()
import Wizard.Api.Resource.Template.TemplateStateSM ()
import Wizard.Database.Migration.Development.Template.Data.Templates

instance ToSchema TemplateDetailDTO where
  declareNamedSchema = simpleToSchema commonWizardTemplateDetailDTO
