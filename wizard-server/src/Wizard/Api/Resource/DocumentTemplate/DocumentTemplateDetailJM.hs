module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
