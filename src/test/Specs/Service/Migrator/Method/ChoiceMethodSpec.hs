module Specs.Service.Migrator.Method.ChoiceMethodSpec where

import Control.Lens
import Data.Maybe
import Test.Hspec

import Model.Event.Event
import Model.Migrator.MigrationState
import Service.Migrator.Methods.ChoiceMethod

import Fixtures.Event.Events
import Fixtures.Migrator.MigrationState

choiceMethodSpec =
  describe "Choice Method" $ do
    it "Test condition 1 - Edit in Core, Edit in localization" $ do
      let parentEvents =
            [ AddKnowledgeModelEvent' a_km1
            , AddChapterEvent' a_km1_ch1
            , AddQuestionEvent' a_km1_ch1_q1
            , AddQuestionEvent' a_km1_ch1_q2
            , EditQuestionEvent' e_km1_ch1_q2
            ]
      let localizationEvents = [EditQuestionEvent' e_km1_ch1_q2_second_edit]
      let ms = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
      let event = EditQuestionEvent' e_km1_ch1_q2_second_edit
      isChoice ms event `shouldBe` True
    it "Test condition 1 - Delete in Core, Edit in localization" $ do
      let parentEvents =
            [ AddKnowledgeModelEvent' a_km1
            , AddChapterEvent' a_km1_ch1
            , AddQuestionEvent' a_km1_ch1_q1
            , AddQuestionEvent' a_km1_ch1_q2
            , DeleteQuestionEvent' d_km1_ch1_q2
            ]
      let localizationEvents = [EditQuestionEvent' e_km1_ch1_q2_second_edit]
      let ms = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
      let event = EditQuestionEvent' e_km1_ch1_q2_second_edit
      isChoice ms event `shouldBe` True
    it "Test condition 2 - Edit in Core, Delete in localization" $ do
      let parentEvents =
            [ AddKnowledgeModelEvent' a_km1
            , AddChapterEvent' a_km1_ch1
            , AddQuestionEvent' a_km1_ch1_q1
            , AddQuestionEvent' a_km1_ch1_q2
            , EditQuestionEvent' e_km1_ch1_q2
            ]
      let localizationEvents = [DeleteQuestionEvent' d_km1_ch1_q2]
      let ms = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
      let event = DeleteQuestionEvent' d_km1_ch1_q2
      isChoice ms event `shouldBe` True
