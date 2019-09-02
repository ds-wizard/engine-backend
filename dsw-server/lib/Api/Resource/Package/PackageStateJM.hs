module Api.Resource.Package.PackageStateJM where

import Data.Aeson

import Model.Package.PackageState

instance FromJSON PackageState

instance ToJSON PackageState
