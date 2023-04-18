module Wizard.Api.Resource.Package.Publish.PackagePublishBranchJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO

instance FromJSON PackagePublishBranchDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePublishBranchDTO where
  toJSON = genericToJSON jsonOptions
