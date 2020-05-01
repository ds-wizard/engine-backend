module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM ()
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations

instance ToSchema MigratorStateCreateDTO where
  declareNamedSchema = simpleToSchema migratorStateCreate
