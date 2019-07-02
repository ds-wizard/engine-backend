module Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO where

import qualified Data.UUID as U

import Api.Resource.Event.EventDTO
import Model.Migration.KnowledgeModel.MigratorState

data MigratorConflictDTO = MigratorConflictDTO
  { _migratorConflictDTOOriginalEventUuid :: U.UUID
  , _migratorConflictDTOAction :: MigrationConflictAction
  , _migratorConflictDTOEvent :: Maybe EventDTO
  } deriving (Show, Eq)
