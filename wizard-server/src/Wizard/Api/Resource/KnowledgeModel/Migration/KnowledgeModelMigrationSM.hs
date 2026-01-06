module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationJM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateSM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations

instance ToSchema KnowledgeModelMigrationDTO where
  declareNamedSchema = toSwagger knowledgeModelMigrationDTO
