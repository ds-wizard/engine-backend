module Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

instance FromJSON DocumentTemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions
