module Wizard.Api.Resource.Package.Publish.PackagePublishMigrationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO

instance FromJSON PackagePublishMigrationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePublishMigrationDTO where
  toJSON = genericToJSON jsonOptions
