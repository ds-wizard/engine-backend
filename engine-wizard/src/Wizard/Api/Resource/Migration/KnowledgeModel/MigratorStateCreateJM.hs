module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON = simpleParseJSON "_migratorStateCreateDTO"

instance ToJSON MigratorStateCreateDTO where
  toJSON = simpleToJSON "_migratorStateCreateDTO"
