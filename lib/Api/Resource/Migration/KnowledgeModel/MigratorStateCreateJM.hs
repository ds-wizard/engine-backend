module Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM where

import Data.Aeson

import Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateCreateDTO where
  parseJSON = simpleParseJSON "_migratorStateCreateDTO"

instance ToJSON MigratorStateCreateDTO where
  toJSON = simpleToJSON "_migratorStateCreateDTO"
