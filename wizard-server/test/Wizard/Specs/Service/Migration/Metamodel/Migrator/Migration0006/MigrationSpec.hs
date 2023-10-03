module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0006.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0006.MigrationFixtures as Fixtures
import Wizard.Specs.Service.Migration.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.Migration.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration6 input =
  case EventMigrator.migrate Fixtures.defaultContext 6 7 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddKnowledgeModelEvent" $
    runMigration6 Fixtures.addKmEventIn1 `shouldBeJson` Fixtures.addKmEventOut1
  it "migrates EditKnowledgeModelEvent" $
    runMigration6 Fixtures.editKmEventIn1 `shouldBeJson` Fixtures.editKmEventOut1
