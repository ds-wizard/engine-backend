module RegistryLib.Api.Resource.Organization.OrganizationStateJM where

import Data.Aeson

import RegistryLib.Api.Resource.Organization.OrganizationStateDTO
import Shared.Common.Util.Aeson

instance ToJSON OrganizationStateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationStateDTO where
  parseJSON = genericParseJSON jsonOptions
