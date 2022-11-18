module Registry.Api.Resource.Template.TemplateDetailJM where

import Data.Aeson

import Registry.Api.Resource.Template.TemplateDetailDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson

instance FromJSON TemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
