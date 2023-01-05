module Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions
