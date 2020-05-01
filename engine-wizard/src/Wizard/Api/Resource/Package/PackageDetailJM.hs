module Wizard.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageStateJM ()

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON simpleOptions
