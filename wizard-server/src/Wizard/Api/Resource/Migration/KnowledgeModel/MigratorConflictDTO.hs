module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Model.Event.Event

data MigratorConflictDTO = MigratorConflictDTO
  { originalEventUuid :: U.UUID
  , action :: MigrationConflictAction
  , event :: Maybe Event
  }
  deriving (Show, Eq, Generic)
