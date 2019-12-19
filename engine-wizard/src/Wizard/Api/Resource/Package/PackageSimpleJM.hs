module Wizard.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Wizard.Api.Resource.Organization.OrganizationSimpleJM ()
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageStateJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageSimpleDTO where
  parseJSON = simpleParseJSON "_packageSimpleDTO"

instance ToJSON PackageSimpleDTO where
  toJSON = simpleToJSON "_packageSimpleDTO"
