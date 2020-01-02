module Wizard.Service.Migration.KnowledgeModel.Migrator.Migrator where

import Control.Lens
import Data.Either
import Data.Maybe

import LensesConfig
import Shared.Model.Event.Event
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Event.EventMapper
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.Migration.KnowledgeModel.Migrator.CleanerMethod
import Wizard.Service.Migration.KnowledgeModel.Migrator.CorrectorMethod

doMigrate :: IO MigratorState -> Event -> IO MigratorState
doMigrate stateIO event = do
  state <- stateIO
  case state ^. migrationState of
    RunningState ->
      if isCleanerMethod state event
        then runCleanerMethod state event
        else runCorrectorMethod state event
    _ -> return state

migrate :: MigratorState -> IO MigratorState
migrate state =
  case state ^. migrationState of
    RunningState -> do
      newState <- foldl doMigrate (return state) (state ^. targetPackageEvents)
      if newState ^. targetPackageEvents == []
        then return $ newState & migrationState .~ CompletedState
        else return newState
    ConflictState _ -> return state
    ErrorState -> return state
    CompletedState -> return state

solveConflict :: MigratorState -> MigratorConflictDTO -> MigratorState
solveConflict state mcDto =
  case mcDto ^. action of
    MCAApply ->
      let events = tail $ state ^. targetPackageEvents
          targetEvent =
            case state ^. migrationState of
              ConflictState (CorrectorConflict event) -> event
       in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    MCAEdited ->
      let events = tail $ state ^. targetPackageEvents
          targetEvent = fromDTOFn . fromJust $ mcDto ^. event
       in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    MCAReject ->
      let events = tail $ state ^. targetPackageEvents
       in toRunningState . updateEvents events $ state
  where
    toRunningState newState = newState & migrationState .~ RunningState
    updateEvents events newState = newState & targetPackageEvents .~ events
    addToResultEvent event newState = newState & resultEvents .~ ((newState ^. resultEvents) ++ [event])
    createNewKm event newState =
      let eitherNewKm = compile (newState ^. currentKnowledgeModel) [event]
       in if isRight eitherNewKm
            then let (Right newKm) = eitherNewKm
                  in newState & currentKnowledgeModel .~ (Just newKm)
            else let (Left error) = eitherNewKm
                  in newState & migrationState .~ ErrorState
