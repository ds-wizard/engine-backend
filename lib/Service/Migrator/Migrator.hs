module Service.Migrator.Migrator where

import Control.Lens
import Data.Either
import Data.Maybe

import Api.Resource.Migrator.MigratorConflictDTO
import LensesConfig
import Model.Event.Event
import Model.Migrator.MigratorState
import Service.Event.EventMapper
import Service.Migrator.Applicator.Applicator
import Service.Migrator.CleanerMethod
import Service.Migrator.CorrectorMethod

doMigrate :: IO MigratorState -> Event -> IO MigratorState
doMigrate stateIO event = do
  state <- stateIO
  case state ^. migrationState of
    RunningState ->
      if isCleanerMethod state event
        then runCleanerMethod state event
        else runCorrectorMethod state event
    _ -> stateIO

migrate :: MigratorState -> IO MigratorState
migrate state =
  case state ^. migrationState of
    RunningState -> do
      newState <- foldl doMigrate (return state) (state ^. targetPackageEvents)
      if newState ^. targetPackageEvents == []
        then return $ newState & migrationState .~ CompletedState
        else return newState
    ConflictState _ -> return state
    ErrorState _ -> return state
    CompletedState -> return state

solveConflict :: MigratorState -> MigratorConflictDTO -> MigratorState
solveConflict state mcDto =
  case mcDto ^. action of
    MCAApply ->
      let events = tail . getModifiedEvents $ state
          targetEvent = head $ state ^. targetPackageEvents
      in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    MCAEdited ->
      let events = tail . getModifiedEvents $ state
          targetEvent = fromDTOFn . fromJust $ mcDto ^. event
      in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    MCAReject ->
      let events = tail . getModifiedEvents $ state
      in toRunningState . updateEvents events $ state
  where
    getModifiedEvents newState = newState ^. targetPackageEvents
    toRunningState newState = newState & migrationState .~ RunningState
    updateEvents events newState = newState & targetPackageEvents .~ events
    addToResultEvent event newState = newState & resultEvents .~ ((newState ^. resultEvents) ++ [event])
    createNewKm event newState =
      let eitherNewKm = runApplicator (newState ^. currentKnowledgeModel) [event]
      in if isRight eitherNewKm
           then let (Right newKm) = eitherNewKm
                in newState & currentKnowledgeModel .~ (Just newKm)
           else let (Left error) = eitherNewKm
                in newState & migrationState .~ (ErrorState error)
