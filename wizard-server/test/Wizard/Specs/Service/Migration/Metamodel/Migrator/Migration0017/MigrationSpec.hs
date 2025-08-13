module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0017.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0017.MigrationFixtures as Fixtures
import Wizard.Specs.Service.Migration.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.Migration.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration17 input =
  case EventMigrator.migrate Fixtures.defaultContext 17 18 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddIntegrationEvent" $
    runMigration17 Fixtures.addIntegrationEventIn1 `shouldBeJson` Fixtures.addIntegrationEventOut1
  it "migrates EditIntegrationEvent" $
    runMigration17 Fixtures.editIntegrationEventIn1 `shouldBeJson` Fixtures.editIntegrationEventOut1
  it "migrates AddQuestionEvent" $
    runMigration17 Fixtures.addQuestionEventIn1 `shouldBeJson` Fixtures.addQuestionEventOut1
  it "migrates EditQuestionEvent" $
    runMigration17 Fixtures.editQuestionEventIn1 `shouldBeJson` Fixtures.editQuestionEventOut1
