module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

instance ToSchema KnowledgeModelMigrationState where
  declareNamedSchema = toSwaggerWithFlatType "type" RunningKnowledgeModelMigrationState
