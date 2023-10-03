module RegistryLib.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationRoleJM ()
import Shared.Common.Util.Aeson

instance ToJSON OrganizationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationDTO where
  parseJSON = genericParseJSON jsonOptions
