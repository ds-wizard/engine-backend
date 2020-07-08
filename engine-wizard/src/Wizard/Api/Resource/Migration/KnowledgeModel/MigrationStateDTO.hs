module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO where

import GHC.Generics

import Shared.Model.Event.Event

data ConflictDTO =
  CorrectorConflictDTO Event
  deriving (Show, Eq, Generic)

data MigrationStateDTO
  = RunningStateDTO
  | ConflictStateDTO ConflictDTO
  | ErrorStateDTO
  | CompletedStateDTO
  deriving (Show, Eq, Generic)
