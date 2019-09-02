module Api.Resource.Migration.KnowledgeModel.MigratorStateDetailJM where

import Data.Aeson

import Api.Resource.Event.EventJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateDetailDTO where
  parseJSON = simpleParseJSON "_migratorStateDetailDTO"

instance ToJSON MigratorStateDetailDTO where
  toJSON = simpleToJSON "_migratorStateDetailDTO"
