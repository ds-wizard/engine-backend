module Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.MigrationSpec where

import Control.Lens
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Event
import Shared.Model.Event.EventAccessors
import Shared.Model.Event.EventField
import Shared.Model.Event.Question.QuestionEvent
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.Migration.KnowledgeModel.Migrator.Migrator

import Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.Common

migratorSpec =
  describe "Migrator" $ do
    describe "Situations (Core <?> Localization)" $ do
      it "Situation n.1: Add < Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target package events
        let msTargetPackageEvents = [AddQuestionEvent' a_km1_ch1_q1']
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.2: Edit < Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target package events
        let msTargetPackageEvents = [EditQuestionEvent' e_km1_ch1_q1']
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        let expEvent = e_km1_ch1_q1 & uuid .~ (getEventUuid resEvent)
        let expState =
              reqState &
              migrationState .~
              ConflictState (CorrectorConflict (EditQuestionEvent' (EditValueQuestionEvent' expEvent)))
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.3: Delete < Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.4: Edit = Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents =
              [EditChapterEvent' e_km1_ch1, AddQuestionEvent' a_km1_ch1_q1', AddQuestionEvent' a_km1_ch1_q2']
        -- And: Prepare target package events
        let msTargetPackageEvents = [EditChapterEvent' e_km1_ch1_2]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        let expEvent = e_km1_ch1_2 & uuid .~ (resEvent ^. uuid)
        let expState = reqState & migrationState .~ ConflictState (CorrectorConflict (EditChapterEvent' expEvent))
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.5: Delete = Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.6: Add = Add (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [AddQuestionEvent' a_km1_ch1_q1']
        -- And: Prepare target package events
        let msTargetPackageEvents = [AddQuestionEvent' a_km1_ch1_q2']
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.7: Edit > Add (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [AddQuestionEvent' a_km1_ch1_q1', AddQuestionEvent' a_km1_ch1_q2']
        -- And: Prepare target package events
        let msTargetPackageEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        let expEvent = e_km1_ch1 & uuid .~ (resEvent ^. uuid)
        let expState = reqState & migrationState .~ ConflictState (CorrectorConflict (EditChapterEvent' expEvent))
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.8: Edit > Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [EditQuestionEvent' e_km1_ch1_q1']
        -- And: Prepare target package events
        let msTargetPackageEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents =
              [ AddKnowledgeModelEvent' a_km1
              , AddChapterEvent' a_km1_ch1
              , AddQuestionEvent' a_km1_ch1_q1'
              , AddQuestionEvent' a_km1_ch1_q2'
              ]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        let expEvent = e_km1_ch1 & uuid .~ (resEvent ^. uuid)
        let expState = reqState & migrationState .~ ConflictState (CorrectorConflict (EditChapterEvent' expEvent))
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.9: Edit > Delete (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
        -- And: Prepare target package events
        let msTargetPackageEvents = [EditChapterEvent' e_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents =
              [ AddKnowledgeModelEvent' a_km1
              , AddChapterEvent' a_km1_ch1
              , AddQuestionEvent' a_km1_ch1_q1'
              , AddQuestionEvent' a_km1_ch1_q2'
              ]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        let expEventEntityUuids = ChangedValue [a_km1_ch1_q2 ^. entityUuid]
        let expEvent = (e_km1_ch1 & uuid .~ (resEvent ^. uuid)) & questionUuids .~ expEventEntityUuids
        let expState = reqState & migrationState .~ ConflictState (CorrectorConflict (EditChapterEvent' expEvent))
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.10: Delete > Delete (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
      -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.11: Delete > Add (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [AddQuestionEvent' a_km1_ch1_q1']
      -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1]
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.12: Delete > Edit (Corrector)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [EditQuestionEvent' e_km1_ch1_q1']
      -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteChapterEvent' d_km1_ch1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = reqState & migrationState .~ (ConflictState . CorrectorConflict . head $ msTargetPackageEvents)
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.13: Delete < Delete (Cleaner)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteChapterEvent' d_km1_ch1]
      -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = (reqState & migrationState .~ CompletedState) & targetPackageEvents .~ []
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.14: Add < Delete (Cleaner)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteChapterEvent' d_km1_ch1]
      -- And: Prepare target package events
        let msTargetPackageEvents = [AddQuestionEvent' a_km1_ch1_q2']
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = (reqState & migrationState .~ CompletedState) & targetPackageEvents .~ []
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.15: Edit < Delete (Cleaner)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteChapterEvent' d_km1_ch1]
      -- And: Prepare target package events
        let msTargetPackageEvents = [EditQuestionEvent' e_km1_ch1_q1']
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = (reqState & migrationState .~ CompletedState) & targetPackageEvents .~ []
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.16: Delete = Delete (Cleaner)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
      -- And: Prepare target package events
        let msTargetPackageEvents = [DeleteQuestionEvent' d_km1_ch1_q1_2]
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = (reqState & migrationState .~ CompletedState) & targetPackageEvents .~ []
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.17: Edit = Delete (Cleaner)" $
        -- Given: Prepare current branch events
       do
        let branchEvents = [DeleteQuestionEvent' d_km1_ch1_q1]
      -- And: Prepare target package events
        let msTargetPackageEvents = [EditQuestionEvent' e_km1_ch1_q1']
        -- And: Prepare current Knowledge Model
        let kmEvents = [AddKnowledgeModelEvent' a_km1, AddChapterEvent' a_km1_ch1, AddQuestionEvent' a_km1_ch1_q1']
        let (Right km) = compile Nothing (kmEvents ++ branchEvents)
        -- And: Prepare current state
        let reqState = createTestMigratorStateWithEvents branchEvents msTargetPackageEvents (Just km)
        -- And: Prepare expected state
        let expState = (reqState & migrationState .~ CompletedState) & targetPackageEvents .~ []
        -- When:
        resState <- migrate reqState
        -- Then:
        resState `shouldBe` expState
