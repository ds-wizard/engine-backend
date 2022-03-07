module TestFixtures where

import Wizard.Metamodel.Migration.MigrationContext

defaultContext :: MigrationContext
defaultContext = MigrationContext {_migrationContextCreatedAt = read "2022-01-01 12:00:00.000000 UTC"}
