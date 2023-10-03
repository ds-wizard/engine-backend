module RegistryLib.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Util.Aeson

instance FromJSON OrganizationSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OrganizationSimple where
  toJSON = genericToJSON jsonOptions
