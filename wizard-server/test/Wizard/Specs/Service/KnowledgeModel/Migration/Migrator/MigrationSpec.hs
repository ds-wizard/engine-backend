module Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.MigrationSpec where

import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Compiler.Compiler
import Wizard.Service.KnowledgeModel.Migration.Migrator.Migrator

import Wizard.Specs.Common
import Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.Common

migratorSpec appContext =
  describe "Migrator" $
    describe "Situations (Core <?> Localization)" $ do
      it "Situation n.1: Add < Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [a_km1_ch1_q1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.2: Edit < Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1_q1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
          let expEvent = e_km1_ch1_q1 {uuid = resEvent.uuid}
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just $ expEvent}
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.3: Delete < Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1_q1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.4: Edit = Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1, a_km1_ch1_q1, a_km1_ch1_q2]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1_2]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
          let expEvent = e_km1_ch1_2 {uuid = resEvent.uuid}
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just $ expEvent}
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.5: Delete = Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.6: Add = Add (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [a_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [a_km1_ch1_q2]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.7: Edit > Add (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [a_km1_ch1_q1, a_km1_ch1_q2]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
          let expEvent = e_km1_ch1 {uuid = resEvent.uuid}
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just $ expEvent}
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.8: Edit > Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents =
                [ a_km1
                , a_km1_ch1
                , a_km1_ch1_q1
                , a_km1_ch1_q2
                ]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
          let expEvent = e_km1_ch1 {uuid = resEvent.uuid}
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just $ expEvent}
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.9: Edit > Delete (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents =
                [ a_km1
                , a_km1_ch1
                , a_km1_ch1_q1
                , a_km1_ch1_q2
                ]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState =
                reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
          let expEventEntityUuids = ChangedValue [a_km1_ch1_q2.entityUuid]
          let (EditChapterEvent' expEventContent) = e_km1_ch1.content
          let expEvent = e_km1_ch1 {uuid = resEvent.uuid, content = EditChapterEvent' $ expEventContent {questionUuids = expEventEntityUuids}}
          let expState =
                reqState
                  { state = ConflictKnowledgeModelMigrationState . Just $ expEvent
                  , targetPackageEvents = [expEvent]
                  }
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.10: Delete > Delete (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.11: Delete > Add (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [a_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.12: Delete > Edit (Corrector)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [e_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = ConflictKnowledgeModelMigrationState . Just . head $ msTargetPackageEvents}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.13: Delete < Delete (Cleaner)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1_q1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = CompletedKnowledgeModelMigrationState, targetPackageEvents = []}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.14: Add < Delete (Cleaner)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [a_km1_ch1_q2]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = CompletedKnowledgeModelMigrationState, targetPackageEvents = []}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.15: Edit < Delete (Cleaner)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1_q1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = CompletedKnowledgeModelMigrationState, targetPackageEvents = []}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.16: Delete = Delete (Cleaner)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [d_km1_ch1_q1_2]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = CompletedKnowledgeModelMigrationState, targetPackageEvents = []}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      it "Situation n.17: Edit = Delete (Cleaner)" $
        -- Given: Prepare current KM editor events
        do
          let editorEvents = [d_km1_ch1_q1]
          -- And: Prepare target package events
          let msTargetPackageEvents = [e_km1_ch1_q1]
          -- And: Prepare current Knowledge Model
          let kmEvents = [a_km1, a_km1_ch1, a_km1_ch1_q1]
          let (Right km) = compile Nothing (kmEvents ++ editorEvents)
          -- And: Prepare current state
          let reqState = createTestMigratorStateWithEvents editorEvents msTargetPackageEvents (Just km)
          -- And: Prepare expected state
          let expState = reqState {state = CompletedKnowledgeModelMigrationState, targetPackageEvents = []}
          -- When:
          (Right resState) <- runInContext (migrate reqState) appContext
          -- Then:
          resState `shouldBe` expState
