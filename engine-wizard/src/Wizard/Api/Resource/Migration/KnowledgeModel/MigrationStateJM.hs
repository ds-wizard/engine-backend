module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO

instance ToJSON MigrationStateDTO where
  toJSON RunningStateDTO = object ["stateType" .= "RunningState"]
  toJSON (ConflictStateDTO (CorrectorConflictDTO event)) =
    object ["stateType" .= "ConflictState", "targetEvent" .= event]
  toJSON ErrorStateDTO = object ["stateType" .= "ErrorState"]
  toJSON CompletedStateDTO = object ["stateType" .= "CompletedState"]

instance FromJSON MigrationStateDTO where
  parseJSON (Object o) = do
    stateType <- o .: "stateType"
    case stateType of
      "RunningState" -> return RunningStateDTO
      "ConflictState" -> do
        event <- o .: "event"
        return . ConflictStateDTO . CorrectorConflictDTO $ event
      "ErrorState" -> return ErrorStateDTO
      "CompletedState" -> return CompletedStateDTO
      _ -> fail "Unsupported migration state type"
  parseJSON _ = mzero
