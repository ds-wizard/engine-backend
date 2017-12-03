module Specs.Service.Migrator.MigratorSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Model.Event.Event
import Model.Event.Question.EditQuestionEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState
import Service.Migrator.Applicator
import Service.Migrator.Migrator

import Fixtures.Event.Events
import Fixtures.KnowledgeModel.Chapters
import Fixtures.KnowledgeModel.KnowledgeModels
import Fixtures.KnowledgeModel.Questions

createTestMigratorStateWithEvents :: [Event] -> [Event] -> Maybe KnowledgeModel -> MigratorState
createTestMigratorStateWithEvents branchEvents targetPackageEvents mKm =
  MigratorState
  { _msBranchUuid = fromJust . U.fromString $ "09080ce7-f513-4493-9583-dce567b8e9c5"
  , _msMigrationState = RunningState
  , _msBranchParentId = "b"
  , _msTargetPackageId = "t"
  , _msBranchEvents = branchEvents
  , _msTargetPackageEvents = targetPackageEvents
  , _msResultEvents = []
  , _msCurrentKnowledgeModel = mKm
  }

migratorSpec =
  describe "Migrator" $
  describe "Situations (Core <?> Localization)" $ do
    it "Situation n.1: Add < Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [AddQuestionEvent' a_km1_ch1_q1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.2: Edit < Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [EditQuestionEvent' e_km1_ch1_q1_title]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.3: Delete < Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.4: Edit = Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [EditChapterEvent' e_km1_ch1_2]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.5: Delete = Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.6: Add = Add (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [AddQuestionEvent' a_km1_ch1_q1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [AddQuestionEvent' a_km1_ch1_q2]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.7: Edit > Add (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [AddQuestionEvent' a_km1_ch1_q1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.8: Edit > Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditQuestionEvent' e_km1_ch1_q1_title]
        -- And: Prepare target parent package events
      let targetPackageEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.9: Edit > Delete (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
        -- And: Prepare target parent package events
      let targetPackageEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.10: Delete > Delete (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.11: Delete > Add (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [AddQuestionEvent' a_km1_ch1_q1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.12: Delete > Edit (Corrector)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [EditQuestionEvent' e_km1_ch1_q1_title]
      -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = reqState & msMigrationState .~ (ConflictState . CorrectorConflict . head $ targetPackageEvents)
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.13: Delete < Delete (Cleaner)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteChapterEvent' d_km1_ch1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = (reqState & msMigrationState .~ CompletedState) & msTargetPackageEvents .~ []
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.14: Add < Delete (Cleaner)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteChapterEvent' d_km1_ch1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [AddQuestionEvent' a_km1_ch1_q2]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = (reqState & msMigrationState .~ CompletedState) & msTargetPackageEvents .~ []
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.15: Edit < Delete (Cleaner)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteChapterEvent' d_km1_ch1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [EditQuestionEvent' e_km1_ch1_q1_title]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = (reqState & msMigrationState .~ CompletedState) & msTargetPackageEvents .~ []
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.16: Delete = Delete (Cleaner)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [DeleteQuestionEvent' d_km1_ch1_q1_2]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = (reqState & msMigrationState .~ CompletedState) & msTargetPackageEvents .~ []
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
    it "Situation n.17: Edit = Delete (Cleaner)" $
        -- Given: Prepare current branch events
     do
      let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
      -- And: Prepare target parent package events
      let targetPackageEvents = [EditQuestionEvent' e_km1_ch1_q1_title]
        -- And: Prepare current Knowledge Model
      let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1]
      let (Right km) = runApplicator Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
      let reqState = createTestMigratorStateWithEvents branchEvents targetPackageEvents (Just km)
        -- And: Prepare expected state
      let expState = (reqState & msMigrationState .~ CompletedState) & msTargetPackageEvents .~ []
        -- When:
      let resState = migrate reqState
        -- Then:
      resState `shouldBe` expState
