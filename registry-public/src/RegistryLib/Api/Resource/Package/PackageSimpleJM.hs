module RegistryLib.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import Shared.Common.Util.Aeson

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON jsonOptions
