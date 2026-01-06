module Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO

instance FromJSON ProjectMigrationCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectMigrationCreateDTO where
  toJSON = genericToJSON jsonOptions
