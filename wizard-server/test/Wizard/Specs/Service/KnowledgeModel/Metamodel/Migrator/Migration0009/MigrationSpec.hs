module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0009.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0009.MigrationFixtures as Fixtures
import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration9 input =
  case EventMigrator.migrate Fixtures.defaultContext 9 10 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddIntegrationEvent" $
    runMigration9 Fixtures.addIntegrationEventIn1 `shouldBeJson` Fixtures.addIntegrationEventOut1
  it "migrates EditIntegrationEvent" $
    runMigration9 Fixtures.editIntegrationEventIn1 `shouldBeJson` Fixtures.editIntegrationEventOut1
