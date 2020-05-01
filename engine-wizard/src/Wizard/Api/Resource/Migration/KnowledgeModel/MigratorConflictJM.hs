module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationConflictActionJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO

instance FromJSON MigratorConflictDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MigratorConflictDTO where
  toJSON = genericToJSON simpleOptions
