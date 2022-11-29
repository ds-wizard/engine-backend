module Registry.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import Registry.Api.Resource.Organization.Common ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Shared.Util.Aeson

instance ToJSON OrganizationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationDTO where
  parseJSON = genericParseJSON jsonOptions
