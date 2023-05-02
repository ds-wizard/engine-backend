module Registry.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON jsonOptions
