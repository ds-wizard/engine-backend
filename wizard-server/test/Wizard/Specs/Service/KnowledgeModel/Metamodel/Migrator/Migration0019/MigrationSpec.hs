module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0019.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0019.MigrationFixtures as Fixtures
import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration19 input =
  case EventMigrator.migrate Fixtures.defaultContext 19 20 input of
    (Left msg) -> error msg
    (Right res) -> res

runMigration19' input =
  case EventMigrator.migrate Fixtures.defaultContext 19 20 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddIntegrationEvent (ApiLegacyIntegration)" $
    runMigration19' Fixtures.addIntegrationEventIn1 `shouldBeJson` Fixtures.addIntegrationEventOut1
  it "migrates EditIntegrationEvent (ApiLegacyIntegration)" $
    runMigration19' Fixtures.editIntegrationEventIn1 `shouldBeJson` Fixtures.editIntegrationEventOut1
  it "migrates AddIntegrationEvent (WidgetIntegration)" $
    runMigration19 Fixtures.addIntegrationEventIn2 `shouldBe` []
  it "migrates EditIntegrationEvent (WidgetIntegration)" $
    runMigration19 Fixtures.editIntegrationEventIn2 `shouldBe` []
  it "migrates AddIntegrationEvent (ApiIntegration)" $
    runMigration19' Fixtures.addIntegrationEventIn3 `shouldBeJson` Fixtures.addIntegrationEventIn3
  it "migrates EditIntegrationEvent (ApiIntegration)" $
    runMigration19' Fixtures.editIntegrationEventIn3 `shouldBeJson` Fixtures.editIntegrationEventIn3
