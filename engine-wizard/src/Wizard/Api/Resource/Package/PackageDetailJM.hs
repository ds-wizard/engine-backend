module Wizard.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePhaseJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageStateJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON jsonOptions
