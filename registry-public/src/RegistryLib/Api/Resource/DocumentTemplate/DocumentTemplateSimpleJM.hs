module RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Common.Util.Aeson

instance FromJSON DocumentTemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions
