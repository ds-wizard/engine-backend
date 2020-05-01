module Registry.Api.Resource.Organization.OrganizationCreateJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Shared.Util.JSON

instance ToJSON OrganizationCreateDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON OrganizationCreateDTO where
  parseJSON = genericParseJSON simpleOptions
