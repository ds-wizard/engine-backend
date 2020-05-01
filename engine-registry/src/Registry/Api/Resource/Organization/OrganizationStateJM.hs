module Registry.Api.Resource.Organization.OrganizationStateJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationStateDTO
import Shared.Util.JSON

instance ToJSON OrganizationStateDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON OrganizationStateDTO where
  parseJSON = genericParseJSON simpleOptions
