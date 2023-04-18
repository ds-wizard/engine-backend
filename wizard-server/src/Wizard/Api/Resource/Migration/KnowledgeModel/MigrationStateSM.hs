module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateSM where

import Data.Swagger

import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema Conflict

instance ToSchema MigrationState where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
