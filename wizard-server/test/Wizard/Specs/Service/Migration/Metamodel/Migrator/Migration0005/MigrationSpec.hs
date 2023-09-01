module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0005.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0005.MigrationFixtures as Fixtures
import Wizard.Specs.Service.Migration.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.Migration.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration5 input =
  case EventMigrator.migrate Fixtures.defaultContext 5 6 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddKnowledgeModelEvent (no change)" $
    runMigration5 Fixtures.addKmEventIn1 `shouldBeJson` Fixtures.addKmEventOut1
