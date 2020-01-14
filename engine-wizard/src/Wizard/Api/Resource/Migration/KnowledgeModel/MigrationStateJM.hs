module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Event.EventMapper

instance ToJSON MigrationStateDTO where
  toJSON RunningStateDTO = object ["stateType" .= "RunningState"]
  toJSON (ConflictStateDTO (CorrectorConflict event)) =
    object ["stateType" .= "ConflictState", "targetEvent" .= (toJSON . toDTOFn $ event)]
  toJSON ErrorStateDTO = object ["stateType" .= "ErrorState"]
  toJSON CompletedStateDTO = object ["stateType" .= "CompletedState"]

instance FromJSON MigrationStateDTO where
  parseJSON (Object o) = do
    stateType <- o .: "stateType"
    case stateType of
      "RunningState" -> return RunningStateDTO
      "ConflictState" -> do
        event <- o .: "event"
        return . ConflictStateDTO . CorrectorConflict . fromDTOFn $ event
      "ErrorState" -> return ErrorStateDTO
      "CompletedState" -> return CompletedStateDTO
      _ -> fail "Unsupported migration state type"
  parseJSON _ = mzero
