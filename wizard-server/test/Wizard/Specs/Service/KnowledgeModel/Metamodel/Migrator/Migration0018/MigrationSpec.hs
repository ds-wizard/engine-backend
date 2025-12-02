module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0018.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0018.MigrationFixtures as Fixtures
import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration18 input =
  case EventMigrator.migrate Fixtures.defaultContext 18 19 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "moves non-top-level fields under content key" $
    runMigration18 Fixtures.oldEvent `shouldBeJson` Fixtures.newEvent
