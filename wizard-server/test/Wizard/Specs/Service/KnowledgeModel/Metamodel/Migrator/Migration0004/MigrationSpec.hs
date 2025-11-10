module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0004.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0004.MigrationFixtures as Fixtures
import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration4 input =
  case EventMigrator.migrate Fixtures.defaultContext 4 5 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddKnowledgeModelEvent (no change)" $
    runMigration4 Fixtures.addKmEventIn1 `shouldBeJson` Fixtures.addKmEventOut1
