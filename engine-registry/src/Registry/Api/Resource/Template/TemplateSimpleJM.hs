module Registry.Api.Resource.Template.TemplateSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson

instance FromJSON TemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions
