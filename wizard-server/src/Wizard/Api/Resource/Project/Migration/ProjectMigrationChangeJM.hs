module Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO

instance FromJSON ProjectMigrationChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectMigrationChangeDTO where
  toJSON = genericToJSON jsonOptions
