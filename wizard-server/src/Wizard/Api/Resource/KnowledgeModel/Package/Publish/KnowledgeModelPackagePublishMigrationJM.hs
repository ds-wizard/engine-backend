module Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO

instance FromJSON PackagePublishMigrationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePublishMigrationDTO where
  toJSON = genericToJSON jsonOptions
