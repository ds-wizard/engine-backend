module Shared.Api.Resource.TemplateBundle.TemplateBundleSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Api.Resource.TemplateBundle.TemplateBundleDTO
import Shared.Api.Resource.TemplateBundle.TemplateBundleJM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Service.TemplateBundle.TemplateBundleMapper
import Shared.Util.Swagger

instance ToSchema TemplateBundleDTO where
  declareNamedSchema = simpleToSchema (toTemplateBundle commonWizardTemplate [] [])
