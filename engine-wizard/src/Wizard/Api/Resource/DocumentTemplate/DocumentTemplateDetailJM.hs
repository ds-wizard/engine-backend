module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()

instance FromJSON DocumentTemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
