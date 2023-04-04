module Wizard.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePhaseJM ()
import Shared.Model.Package.PackageSimple
import Shared.Util.Aeson
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageStateJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON PackageSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimple where
  toJSON = genericToJSON jsonOptions
