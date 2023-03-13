module Wizard.Api.Resource.Package.PackageChangeJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePhaseJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Package.PackageChangeDTO

instance FromJSON PackageChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageChangeDTO where
  toJSON = genericToJSON jsonOptions
