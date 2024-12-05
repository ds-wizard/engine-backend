module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0016.MigrationSpec (
  spec,
) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0016.MigrationFixtures as Fixtures
import Wizard.Specs.Service.Migration.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.Migration.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration16 input =
  case EventMigrator.migrate Fixtures.defaultContext 16 17 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddQuestionEvent (ValueQuestion)" $
    runMigration16 Fixtures.eventIn0 `shouldBeJson` Fixtures.eventOut0
  it "migrates EditReferenceEvent (ValueQuestion)" $
    runMigration16 Fixtures.eventIn1 `shouldBeJson` Fixtures.eventOut1
  it "migrates AddQuestionEvent (OptionsQuestion, unchanged)" $
    runMigration16 Fixtures.eventIn2 `shouldBeJson` Fixtures.eventOut2
  it "migrates EditReferenceEvent (FileQuestion, unchanged)" $
    runMigration16 Fixtures.eventIn3 `shouldBeJson` Fixtures.eventOut3
