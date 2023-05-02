module Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

instance ToJSON MigrationState where
  toJSON RunningState = object ["stateType" .= "RunningState"]
  toJSON (ConflictState (CorrectorConflict event)) = object ["stateType" .= "ConflictState", "targetEvent" .= event]
  toJSON ErrorState = object ["stateType" .= "ErrorState"]
  toJSON CompletedState = object ["stateType" .= "CompletedState"]

instance FromJSON MigrationState where
  parseJSON (Object o) = do
    stateType <- o .: "stateType"
    case stateType of
      "RunningState" -> return RunningState
      "ConflictState" -> return . ConflictState . CorrectorConflict $ Nothing
      "ErrorState" -> return ErrorState
      "CompletedState" -> return CompletedState
      _ -> fail "Unsupported migration state type"
  parseJSON _ = mzero
