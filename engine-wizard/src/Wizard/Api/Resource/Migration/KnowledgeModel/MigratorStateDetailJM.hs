module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO

instance FromJSON MigratorStateDetailDTO where
  parseJSON = simpleParseJSON "_migratorStateDetailDTO"

instance ToJSON MigratorStateDetailDTO where
  toJSON = simpleToJSON "_migratorStateDetailDTO"
