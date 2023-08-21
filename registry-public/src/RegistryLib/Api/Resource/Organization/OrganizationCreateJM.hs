module RegistryLib.Api.Resource.Organization.OrganizationCreateJM where

import Data.Aeson

import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import Shared.Common.Util.Aeson

instance ToJSON OrganizationCreateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationCreateDTO where
  parseJSON = genericParseJSON jsonOptions
