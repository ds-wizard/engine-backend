module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()

instance ToSchema ConflictDTO

instance ToSchema MigrationStateDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
