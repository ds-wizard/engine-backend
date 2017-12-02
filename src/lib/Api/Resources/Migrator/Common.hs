module Api.Resources.Migrator.Common where

import Control.Monad
import Data.Aeson

import Api.Resources.Error.ErrorDTO
import Api.Resources.Event.EventDTO
import Common.Error
import Model.Migrator.MigratorState
import Service.Event.EventMapper

instance ToJSON MigrationState where
  toJSON RunningState = object ["stateType" .= "RunningState"]
  toJSON (ConflictState (CorrectorConflict event)) =
    object ["stateType" .= "ConflictState", "targetEvent" .= (toJSON . toDTOFn $ event)]
  toJSON (ErrorState appError) = object ["stateType" .= "ErrorState", "error" .= toJSON appError]
  toJSON CompletedState = object ["stateType" .= "CompletedState"]

instance FromJSON MigrationState where
  parseJSON (Object o) = do
    stateType <- o .: "stateType"
    case stateType of
      "RunningState" -> return RunningState
      "ConflictState" -> do
        event <- o .: "event"
        return . ConflictState . CorrectorConflict . fromDTOFn $ event
      "ErrorState" -> do
        error <- o .: "errror"
        return $ ErrorState error
      "CompletedState" -> return CompletedState
      _ -> fail "Unsupported migration state type"
  parseJSON _ = mzero
