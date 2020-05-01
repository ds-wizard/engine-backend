module Registry.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import Registry.Api.Resource.Organization.Common ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Shared.Util.JSON

instance ToJSON OrganizationDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON OrganizationDTO where
  parseJSON = genericParseJSON simpleOptions
