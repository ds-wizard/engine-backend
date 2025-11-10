module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0014.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0014.MigrationFixtures as Fixtures
import Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration14 input =
  case EventMigrator.migrate Fixtures.defaultContext 14 15 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddReferenceEvent (ResourcePageReference)" $
    runMigration14 Fixtures.eventIn0 `shouldBeJson` Fixtures.eventOut0
  it "migrates EditReferenceEvent (ResourcePageReference)" $
    runMigration14 Fixtures.eventIn1 `shouldBeJson` Fixtures.eventOut1
  it "migrates EditReferenceEvent (URLReference, unchanged)" $
    runMigration14 Fixtures.eventIn2 `shouldBeJson` Fixtures.eventOut2
  it "migrates AddReferenceEvent (CrossReference, unchanged)" $
    runMigration14 Fixtures.eventIn3 `shouldBeJson` Fixtures.eventOut3
  it "migrates EditKnowledgeModelEvent" $
    runMigration14 Fixtures.eventIn4 `shouldBeJson` Fixtures.eventOut4
