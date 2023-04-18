module Migration0008.MigrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Migration0008.MigrationFixtures as Fixtures
import TestFixtures as Fixtures

import WizardLib.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration8 input =
  case EventMigrator.migrate Fixtures.defaultContext 8 9 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddIntegrationEvent" $
    runMigration8 Fixtures.addIntegrationEventIn1 `shouldBeJson` Fixtures.addIntegrationEventOut1
  it "migrates EditIntegrationEvent" $
    runMigration8 Fixtures.editIntegrationEventIn1 `shouldBeJson` Fixtures.editIntegrationEventOut1
