module Registry.Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Shared.Util.JSON

instance ToJSON OrganizationChangeDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON OrganizationChangeDTO where
  parseJSON = genericParseJSON simpleOptions
