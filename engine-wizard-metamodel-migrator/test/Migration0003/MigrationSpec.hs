module Migration0003.MigrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Migration0003.MigrationFixtures as Fixtures
import TestFixtures as Fixtures

import Wizard.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration3 input =
  case EventMigrator.migrate Fixtures.defaultContext 3 4 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddKnowledgeModelEvent" $
    runMigration3 Fixtures.addKmEventIn1 `shouldBeJson` Fixtures.addKmEventOut1
  it "migrates EditKnowledgeModelEvent" $
    runMigration3 Fixtures.editKmEventIn1 `shouldBeJson` Fixtures.editKmEventOut1
  it "migrates DeleteQuestionEevent" $
    runMigration3 Fixtures.deleteQuestionEventIn1 `shouldBeJson` Fixtures.deleteQuestionEventOut1
