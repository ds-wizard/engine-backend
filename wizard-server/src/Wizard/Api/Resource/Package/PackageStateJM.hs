module Wizard.Api.Resource.Package.PackageStateJM where

import Data.Aeson

import Wizard.Model.Package.PackageState

instance FromJSON PackageState

instance ToJSON PackageState
