module Service.Migrator.Methods.CorrectorMethod where

import Control.Lens

import Model.Event.Event
import Model.Migrator.MigratorState

runCorrectorMethod :: MigratorState -> Event -> MigratorState
runCorrectorMethod state event = state & msMigrationState .~ (ConflictState . CorrectorConflict $ event)
