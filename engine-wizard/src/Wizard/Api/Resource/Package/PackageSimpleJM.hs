module Wizard.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Model.Package.PackageSimple
import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageStateJM ()

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON PackageSimple where
  parseJSON = simpleParseJSON "_packageSimple"

instance ToJSON PackageSimple where
  toJSON = simpleToJSON "_packageSimple"
