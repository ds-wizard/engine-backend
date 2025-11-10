module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationActionSM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations

instance ToSchema KnowledgeModelMigrationResolutionDTO where
  declareNamedSchema = toSwagger migratorConflict
