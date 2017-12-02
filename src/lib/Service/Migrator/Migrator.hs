module Service.Migrator.Migrator where

import Control.Lens
import Data.Either
import Data.Maybe

import Api.Resources.Migrator.MigratorConflictDTO
import Common.Error
import Model.Event.Event
import Model.Migrator.MigratorState
import Service.Event.EventMapper
import Service.Migrator.Applicator
import Service.Migrator.Methods.CleanerMethod
import Service.Migrator.Methods.CorrectorMethod

doMigrate :: MigratorState -> Event -> MigratorState
doMigrate state event =
  if isCleanerMethod state event
    then runCleanerMethod state event
    else runCorrectorMethod state event

migrate :: MigratorState -> MigratorState
migrate state =
  case state ^. msMigrationState of
    RunningState ->
      let newState = foldl doMigrate state (state ^. msTargetPackageEvents)
      in if newState ^. msTargetPackageEvents == []
           then newState & msMigrationState .~ CompletedState
           else newState
    ConflictState _ -> state
    ErrorState _ -> state
    CompletedState -> state

solveConflictAndMigrate :: MigratorState -> MigratorConflictDTO -> MigratorState
solveConflictAndMigrate state mcDto =
  case mcDto ^. mcdtoAction of
    MCAApply ->
      let events = tail . getModifiedEvents $ state
          targetEvent = head $ state ^. msTargetPackageEvents
      in migrate . createNewKm targetEvent . toRunningState . updateEvents events $ state
    MCAEdited ->
      let events = tail . getModifiedEvents $ state
          targetEvent = fromDTOFn . fromJust $ mcDto ^. mcdtoEvent
      in migrate . createNewKm targetEvent . toRunningState . updateEvents events $ state
    MCAReject ->
      let events = tail . getModifiedEvents $ state
      in migrate . toRunningState . updateEvents events $ state
  where
    getModifiedEvents newState = newState ^. msTargetPackageEvents
    toRunningState newState = newState & msMigrationState .~ RunningState
    updateEvents events newState = newState & msTargetPackageEvents .~ events
    createNewKm event newState =
      let eitherNewKm = runApplicator (newState ^. msCurrentKnowledgeModel) [event]
      in if isRight eitherNewKm
           then let (Right newKm) = eitherNewKm
                in newState & msCurrentKnowledgeModel .~ (Just newKm)
           else let (Left error) = eitherNewKm
                in newState & msMigrationState .~ (ErrorState error)
