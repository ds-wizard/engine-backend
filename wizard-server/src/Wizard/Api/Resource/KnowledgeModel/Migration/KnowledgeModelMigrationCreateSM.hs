module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations

instance ToSchema KnowledgeModelMigrationCreateDTO where
  declareNamedSchema = toSwagger knowledgeModelMigrationCreateDTO
