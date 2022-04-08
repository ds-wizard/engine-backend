module Migration0001.MigrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Migration0001.MigrationFixtures as Fixtures
import TestFixtures as Fixtures

import Wizard.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration1 input =
  case EventMigrator.migrate Fixtures.defaultContext 1 2 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates EditKnowledgeModelEvent" $
    runMigration1 Fixtures.editKmEventIn1 `shouldBeJson` Fixtures.editKmEventOut1
