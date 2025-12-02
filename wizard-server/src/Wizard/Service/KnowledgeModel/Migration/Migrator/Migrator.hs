module Wizard.Service.KnowledgeModel.Migration.Migrator.Migrator where

import Data.Either
import Data.Maybe

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Compiler.Compiler
import Wizard.Service.KnowledgeModel.Migration.Migrator.CleanerMethod
import Wizard.Service.KnowledgeModel.Migration.Migrator.CorrectorMethod

migrate :: KnowledgeModelMigration -> AppContextM KnowledgeModelMigration
migrate state =
  case state.state of
    RunningKnowledgeModelMigrationState -> do
      newState <- foldl doMigrate (return state) state.targetPackageEvents
      if null $ newState.targetPackageEvents
        then return $ newState {state = CompletedKnowledgeModelMigrationState}
        else return newState
    _ -> return state

doMigrate :: AppContextM KnowledgeModelMigration -> KnowledgeModelEvent -> AppContextM KnowledgeModelMigration
doMigrate stateIO event = do
  state <- stateIO
  case state.state of
    RunningKnowledgeModelMigrationState ->
      if isCleanerMethod state event
        then runCleanerMethod state event
        else runCorrectorMethod state event
    _ -> return state

solveConflict :: KnowledgeModelMigration -> KnowledgeModelMigrationResolutionDTO -> KnowledgeModelMigration
solveConflict state mcDto =
  case mcDto.action of
    ApplyKnowledgeModelMigrationAction ->
      let events = tail $ state.targetPackageEvents
          targetEvent =
            case state.state of
              ConflictKnowledgeModelMigrationState event -> fromJust event
              _ -> error "Expected a CorrectorConflict in ApplyKnowledgeModelMigrationAction action"
       in createNewKm targetEvent . toRunningState . updateEvents events . addToResultEvent targetEvent $ state
    RejectKnowledgeModelMigrationAction ->
      let events = tail $ state.targetPackageEvents
       in toRunningState . updateEvents events $ state
  where
    toRunningState newState = newState {state = RunningKnowledgeModelMigrationState}
    updateEvents events newState = newState {targetPackageEvents = events}
    addToResultEvent event newState = newState {resultEvents = newState.resultEvents ++ [event]}
    createNewKm event newState =
      let eitherNewKm = compile newState.currentKnowledgeModel [event]
       in if isRight eitherNewKm
            then
              let (Right newKm) = eitherNewKm
               in newState {currentKnowledgeModel = Just newKm}
            else
              let (Left error) = eitherNewKm
               in newState {state = ErrorKnowledgeModelMigrationState}
