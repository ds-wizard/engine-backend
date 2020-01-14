module Wizard.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Wizard.Api.Resource.Organization.OrganizationSimpleJM ()
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageStateJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageDetailDTO where
  parseJSON = simpleParseJSON "_packageDetailDTO"

instance ToJSON PackageDetailDTO where
  toJSON = simpleToJSON "_packageDetailDTO"
