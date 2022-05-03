module Migration0002.MigrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Migration0002.MigrationFixtures as Fixtures
import TestFixtures as Fixtures

import Wizard.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration2 input =
  case EventMigrator.migrate Fixtures.defaultContext 2 3 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates EditQuestionEvent" $ do
    runMigration2 Fixtures.addQuestionEventIn1 `shouldBeJson` Fixtures.addQuestionEventOut1
    runMigration2 Fixtures.addQuestionEventIn2 `shouldBeJson` Fixtures.addQuestionEventOut2
  it "migrates EditQuestionEvent" $
    runMigration2 Fixtures.editQuestionEventIn1 `shouldBeJson` Fixtures.editQuestionEventOut1
