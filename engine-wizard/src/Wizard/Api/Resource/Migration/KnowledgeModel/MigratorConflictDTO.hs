module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Wizard.Model.Migration.KnowledgeModel.MigratorState

data MigratorConflictDTO =
  MigratorConflictDTO
    { _migratorConflictDTOOriginalEventUuid :: U.UUID
    , _migratorConflictDTOAction :: MigrationConflictAction
    , _migratorConflictDTOEvent :: Maybe Event
    }
  deriving (Show, Eq, Generic)
