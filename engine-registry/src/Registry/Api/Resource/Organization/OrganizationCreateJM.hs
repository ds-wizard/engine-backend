module Registry.Api.Resource.Organization.OrganizationCreateJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Shared.Util.Aeson

instance ToJSON OrganizationCreateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationCreateDTO where
  parseJSON = genericParseJSON jsonOptions
