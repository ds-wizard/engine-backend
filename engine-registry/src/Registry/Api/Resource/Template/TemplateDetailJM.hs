module Registry.Api.Resource.Template.TemplateDetailJM where

import Data.Aeson

import Registry.Api.Resource.Template.TemplateDetailDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON

instance FromJSON TemplateDetailDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateDetailDTO where
  toJSON = genericToJSON simpleOptions
