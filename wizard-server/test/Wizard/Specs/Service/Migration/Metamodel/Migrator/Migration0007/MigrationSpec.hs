module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0007.MigrationSpec (
  spec,
) where

import Data.Aeson
import Data.Vector (fromList)

import Test.Hspec
import Test.Hspec.Expectations.Json

import Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0007.MigrationFixtures as Fixtures
import Wizard.Specs.Service.Migration.Metamodel.Migrator.TestFixtures as Fixtures

import Wizard.Service.Migration.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration7vec :: Value -> Value
runMigration7vec input =
  case EventMigrator.migrate Fixtures.defaultContext 7 8 input of
    (Left msg) -> error msg
    (Right lst) -> Array $ fromList lst

runMigration7one :: Value -> Value
runMigration7one input =
  case EventMigrator.migrate Fixtures.defaultContext 7 8 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddKnowledgeModelEvent" $
    runMigration7vec Fixtures.addKmEventIn1 `shouldBeJson` Fixtures.addKmEventOut1
  it "migrates EditKnowledgeModelEvent" $
    runMigration7one Fixtures.editKmEventIn1 `shouldBeJson` Fixtures.editKmEventOut1
  it "migrates AddQuestionEvent" $ do
    runMigration7one Fixtures.addQuestionEventIn0 `shouldBeJson` Fixtures.addQuestionEventOut0
    runMigration7one Fixtures.addQuestionEventIn1 `shouldBeJson` Fixtures.addQuestionEventOut1
    runMigration7one Fixtures.addQuestionEventIn2 `shouldBeJson` Fixtures.addQuestionEventOut2
  it "migrates EditQuestionEvent" $ do
    runMigration7one Fixtures.editQuestionEventIn0 `shouldBeJson` Fixtures.editQuestionEventOut0
    runMigration7one Fixtures.editQuestionEventIn3 `shouldBeJson` Fixtures.editQuestionEventOut3
