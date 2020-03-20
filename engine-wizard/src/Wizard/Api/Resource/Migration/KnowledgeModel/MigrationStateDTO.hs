module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO where

import GHC.Generics

import Shared.Api.Resource.Event.EventDTO

data ConflictDTO =
  CorrectorConflictDTO EventDTO
  deriving (Show, Eq, Generic)

data MigrationStateDTO
  = RunningStateDTO
  | ConflictStateDTO ConflictDTO
  | ErrorStateDTO
  | CompletedStateDTO
  deriving (Show, Eq, Generic)
