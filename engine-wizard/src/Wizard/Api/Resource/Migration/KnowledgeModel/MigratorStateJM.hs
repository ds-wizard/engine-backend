module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM where

import Data.Aeson

import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateDTO where
  parseJSON = simpleParseJSON "_migratorStateDTO"

instance ToJSON MigratorStateDTO where
  toJSON = simpleToJSON "_migratorStateDTO"
