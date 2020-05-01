module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM ()
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations

instance ToSchema MigratorStateDTO where
  declareNamedSchema = simpleToSchema migratorState
