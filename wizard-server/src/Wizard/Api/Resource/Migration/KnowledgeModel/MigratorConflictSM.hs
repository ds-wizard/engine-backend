module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationConflictActionSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM ()
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema MigratorConflictDTO where
  declareNamedSchema = toSwagger migratorConflict
