module TestFixtures where

import WizardLib.Metamodel.Migration.MigrationContext

defaultContext :: MigrationContext
defaultContext = MigrationContext {ctxCreatedAt = read "2022-01-01 12:00:00.000000 UTC"}
