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

doMigrate :: IO MigratorState -> Event -> IO MigratorState
doMigrate stateIO event = do
  state <- stateIO
  case state ^. msMigrationState of
    RunningState ->
      if isCleanerMethod state event
        then runCleanerMethod state event
        else runCorrectorMethod state event
    _ -> stateIO

migrate :: MigratorState -> IO MigratorState
migrate state =
  case state ^. msMigrationState of
    RunningState -> do
      newState <- foldl doMigrate (return state) (state ^. msTargetPackageEvents)
      if newState ^. msTargetPackageEvents == []
        then return $ newState & msMigrationState .~ CompletedState
        else return newState
    ConflictState _ -> return state
    ErrorState _ -> return state
    CompletedState -> return state

solveConflict :: MigratorState -> MigratorConflictDTO -> MigratorState
solveConflict state mcDto =
  case mcDto ^. mcdtoAction of
    MCAApply ->
      let events = tail . getModifiedEvents $ state
          targetEvent = head $ state ^. msTargetPackageEvents
      in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    MCAEdited ->
      let events = tail . getModifiedEvents $ state
          targetEvent = fromDTOFn . fromJust $ mcDto ^. mcdtoEvent
      in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    MCAReject ->
      let events = tail . getModifiedEvents $ state
      in toRunningState . updateEvents events $ state
  where
    getModifiedEvents newState = newState ^. msTargetPackageEvents
    toRunningState newState = newState & msMigrationState .~ RunningState
    updateEvents events newState = newState & msTargetPackageEvents .~ events
    addToResultEvent event newState = newState & msResultEvents .~ ((newState ^. msResultEvents) ++ [event])
    createNewKm event newState =
      let eitherNewKm = runApplicator (newState ^. msCurrentKnowledgeModel) [event]
      in if isRight eitherNewKm
           then let (Right newKm) = eitherNewKm
                in newState & msCurrentKnowledgeModel .~ (Just newKm)
           else let (Left error) = eitherNewKm
                in newState & msMigrationState .~ (ErrorState error)
