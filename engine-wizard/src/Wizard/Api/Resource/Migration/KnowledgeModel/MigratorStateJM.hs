module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO

instance FromJSON MigratorStateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MigratorStateDTO where
  toJSON = genericToJSON simpleOptions
