module Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleJM ()
import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageStateJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageDetailDTO where
  parseJSON = simpleParseJSON "_packageDetailDTO"

instance ToJSON PackageDetailDTO where
  toJSON = simpleToJSON "_packageDetailDTO"
