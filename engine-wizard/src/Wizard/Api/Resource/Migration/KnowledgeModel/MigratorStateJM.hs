module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO

instance FromJSON MigratorStateDTO where
  parseJSON = simpleParseJSON "_migratorStateDTO"

instance ToJSON MigratorStateDTO where
  toJSON = simpleToJSON "_migratorStateDTO"
