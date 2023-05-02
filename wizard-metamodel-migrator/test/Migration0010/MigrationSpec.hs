module Migration0010.MigrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Json

import Migration0010.MigrationFixtures as Fixtures
import TestFixtures as Fixtures

import WizardLib.Metamodel.Migrator.EventMigrator as EventMigrator

runMigration10 input =
  case EventMigrator.migrate Fixtures.defaultContext 10 11 input of
    (Left msg) -> error msg
    (Right [res]) -> res
    _ -> error "Invalid result"

spec :: Spec
spec = do
  it "migrates AddPhaseEvent (createdAt, annotations)" $
    runMigration10 Fixtures.addPhaseEventIn1 `shouldBeJson` Fixtures.addPhaseEventOut1
  it "migrates DeleteQuestionEvent (createdAt)" $
    runMigration10 Fixtures.deleteQuestionEventIn1 `shouldBeJson` Fixtures.deleteQuestionEventOut1
  it "migrates EditKnowledgeModelEvent (createdAt, annotations)" $
    runMigration10 Fixtures.editKmEventIn1 `shouldBeJson` Fixtures.editKmEventOut1
  it "migrates AddIntegrationEvent (createdAt, annotations, rqHeaders)" $
    runMigration10 Fixtures.addIntegrationEventIn1 `shouldBeJson` Fixtures.addIntegrationEventOut1
  it "migrates EditIntegrationEvent (createdAt, annotations, rqHeaders)" $
    runMigration10 Fixtures.editIntegrationEventIn1 `shouldBeJson` Fixtures.editIntegrationEventOut1
