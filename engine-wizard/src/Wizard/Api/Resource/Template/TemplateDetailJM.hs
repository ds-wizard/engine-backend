module Wizard.Api.Resource.Template.TemplateDetailJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateStateJM ()

instance FromJSON TemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
