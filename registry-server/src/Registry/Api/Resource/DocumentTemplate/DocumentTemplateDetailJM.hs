module Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

instance FromJSON DocumentTemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
