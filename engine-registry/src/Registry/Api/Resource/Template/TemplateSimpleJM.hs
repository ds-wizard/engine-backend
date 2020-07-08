module Registry.Api.Resource.Template.TemplateSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON

instance FromJSON TemplateSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateSimpleDTO where
  toJSON = genericToJSON simpleOptions
