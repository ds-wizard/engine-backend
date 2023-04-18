module Migration0011.MigrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Migration0011.MigrationFixtures as Fixtures
import TestFixtures as Fixtures

import WizardLib.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration11 input =
  case EventMigrator.migrate Fixtures.defaultContext 11 12 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddIntegrationEvent" $
    runMigration11 Fixtures.addIntegrationEventIn1 `shouldBeJson` Fixtures.addIntegrationEventOut1
  it "migrates EditIntegrationEvent" $
    runMigration11 Fixtures.editIntegrationEventIn1 `shouldBeJson` Fixtures.editIntegrationEventOut1
