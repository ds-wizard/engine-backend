module Fixtures.Migrator.MigrationState where

import Control.Lens

import Model.Migrator.MigrationState
import Service.Migrator.Applicator
import Service.Migrator.Migrator

createRunningMigrationStateWithoutPackage parentEvents localizationEvents =
  createMigrationStateWithEvents "..." "..." parentEvents localizationEvents
