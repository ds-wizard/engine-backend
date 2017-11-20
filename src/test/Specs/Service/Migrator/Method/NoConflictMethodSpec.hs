module Specs.Service.Migrator.Method.ChoiceMethodSpec where

import Control.Lens
import Data.Maybe
import Test.Hspec

import Model.Event.Event
import Model.Migrator.MigrationState
import Service.Migrator.Migrator

import Fixtures.Event.Events
import Fixtures.KnowledgeModel.KnowledgeModels
import Fixtures.Migrator.MigrationState
--choiceMethodSpec =
--  describe "No Conflict Method" $ do
--    it "Situation n.1: Add > Add" $ do
--      -- Given: Prepare current state
--      let parentEvents =
--            [ AddKnowledgeModelEvent' a_km1
--            , AddChapterEvent' a_km1_ch1
--            , AddQuestionEvent' a_km1_ch1_q1
--            , AddQuestionEvent' a_km1_ch1_q2
--            ]
--      let localizationEvents = [AddQuestionEvent' a_km1_ch1_q3]
--      let reqState = createRunningMigrationStateWithoutPackage parentEvents localizationEvents
--
--      -- And: Prepare expected state
--      let expKm = km1 & kmChapters .~ [chapter1WithAddedQuestion3]
--      let expState = (reqState & msStatus .~ MSCompleted) & msCurrentKnowledgeModel .~ expKm)
--
--      -- When:
--      let resState = migrate reqState
--
--      -- Then:
--      resState `shouldBe` expState
