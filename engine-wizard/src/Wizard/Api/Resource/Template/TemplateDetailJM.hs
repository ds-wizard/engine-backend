module Wizard.Api.Resource.Template.TemplateDetailJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateStateJM ()

instance FromJSON TemplateDetailDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateDetailDTO where
  toJSON = genericToJSON simpleOptions
