module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Wizard.Model.Migration.KnowledgeModel.MigratorState

data MigratorConflictDTO = MigratorConflictDTO
  { originalEventUuid :: U.UUID
  , action :: MigrationConflictAction
  , event :: Maybe Event
  }
  deriving (Show, Eq, Generic)
