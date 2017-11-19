module Specs.Service.Migrator.MigratorSpec where

import Control.Lens
import Data.Maybe
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Model.Event.Event
import Model.Event.Question.EditQuestionEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigrationState
import Service.Migrator.Migrator

import Specs.Service.Migrator.Method.ChoiceMethodSpec
import Fixtures.KnowledgeModel.KnowledgeModels
import Fixtures.KnowledgeModel.Chapters
import Fixtures.KnowledgeModel.Questions
import Fixtures.Event.Events
import Fixtures.Migrator.MigrationState

migratorSpec = describe "Migrator" $ do
  describe "Methods" $
    choiceMethodSpec
  describe "Situations (Core ? Localization)" $ do
--    it "Situation n.1: Add > Add" $ do
--      -- Given: Prepare current state
--      let parentEvents =
--            [ AddKnowledgeModelEvent' a_km1
--            , AddChapterEvent' a_km1_ch1
--            ]
--      let localizationEvents = [AddQuestionEvent' a_km1_ch1_q1]
--      let reqState = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
--
--      -- And: Prepare expected state
--      let expKm = km1 & kmChapters .~ [chapter1WithoutQuestions & chQuestions .~ [question1]]
--      let expState = ((reqState & msStatus .~ MSCompleted) & msCurrentKnowledgeModel .~ Just expKm) & msLocalizationEvents .~ []
--
--      -- When:
--      let resState = migrate reqState
--
--      -- Then:
--      resState `shouldBe` expState
--    it "Situation n.2: Add > Edit" $ do
--      -- Given: Prepare current state
--      let parentEvents =
--            [ AddKnowledgeModelEvent' a_km1
--            , AddChapterEvent' a_km1_ch1
--            ]
--      let localizationEvents = [AddQuestionEvent' a_km1_ch1_q1, EditQuestionEvent' e_km1_ch1_q1_title]
--      let reqState = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
--
--      -- And: Prepare expected state
--      let expKm = km1 & kmChapters .~ [chapter1WithoutQuestions & chQuestions .~ [question1 & qTitle .~ fromJust (e_km1_ch1_q1_title ^. eqTitle)]]
--      let expState = ((reqState & msStatus .~ MSCompleted) & msCurrentKnowledgeModel .~ Just expKm) & msLocalizationEvents .~ []
--
--      -- When:
--      let resState = migrate reqState
--
--      -- Then:
--      resState `shouldBe` expState
--    it "Situation n.3: Add > Delete" $ do
--      -- Given: Prepare current state
--      let parentEvents =
--            [ AddKnowledgeModelEvent' a_km1
--            , AddChapterEvent' a_km1_ch1
--            ]
--      let localizationEvents = [AddQuestionEvent' a_km1_ch1_q1, DeleteQuestionEvent' d_km1_ch1_q1]
--      let reqState = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
--
--      -- And: Prepare expected state
--      let expKm = km1 & kmChapters .~ [chapter1WithoutQuestions & chQuestions .~ []]
--      let expState = ((reqState & msStatus .~ MSCompleted) & msCurrentKnowledgeModel .~ Just expKm) & msLocalizationEvents .~ []
--
--      -- When:
--      let resState = migrate reqState
--
--      -- Then:
--      resState `shouldBe` expState
    it "Situation n.1: Add < Edit" $ do
      -- Given: Prepare current state
      let parentEvents =
            [ AddKnowledgeModelEvent' a_km1
            , AddChapterEvent' a_km1_ch1
            ]
      let localizationEvents = [AddQuestionEvent' a_km1_ch1_q1, DeleteQuestionEvent' d_km1_ch1_q1]
      let reqState = createRunningMigrationStateWithoutPackage parentEvents localizationEvents

      -- And: Prepare expected state
      let expKm = km1 & kmChapters .~ [chapter1WithoutQuestions & chQuestions .~ []]
      let expState = ((reqState & msStatus .~ MSCompleted) & msCurrentKnowledgeModel .~ Just expKm) & msLocalizationEvents .~ []

      -- When:
      let resState = migrate reqState

      -- Then:
      resState `shouldBe` expState
