module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState

instance ToSchema Conflict

instance ToSchema MigrationState where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
