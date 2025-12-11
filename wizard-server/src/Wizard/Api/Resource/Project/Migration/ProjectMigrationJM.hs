module Wizard.Api.Resource.Project.Migration.ProjectMigrationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO

instance FromJSON ProjectMigrationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectMigrationDTO where
  toJSON = genericToJSON jsonOptions
