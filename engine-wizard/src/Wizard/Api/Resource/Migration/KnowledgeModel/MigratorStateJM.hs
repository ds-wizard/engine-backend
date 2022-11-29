module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO

instance FromJSON MigratorStateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateDTO where
  toJSON = genericToJSON jsonOptions
