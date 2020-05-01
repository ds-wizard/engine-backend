module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationConflictActionSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM ()
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations

instance ToSchema MigratorConflictDTO where
  declareNamedSchema = simpleToSchema migratorConflict
