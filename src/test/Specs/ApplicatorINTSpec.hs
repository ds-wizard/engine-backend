{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Specs.ApplicatorINTSpec where

import Control.Lens
import Data.Maybe
import Test.Hspec (describe, it)
import Test.Hspec.Expectations.Pretty

import Fixtures.Event.Events
import Fixtures.KnowledgeModel.AnswersAndFollowUpQuestions
import Fixtures.KnowledgeModel.Chapters
import Fixtures.KnowledgeModel.Experts
import Fixtures.KnowledgeModel.KnowledgeModels
import Fixtures.KnowledgeModel.Questions
import Fixtures.KnowledgeModel.References
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Common
import Model.Event.Event
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import KMMigration.Migration.Migration
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

applicatorINTSpec =
 describe "Applicator Integration test" $
 it "Apply: Create KM from scratch" $ do
   let events =
         [ AddKnowledgeModelEvent' a_km1
         , AddChapterEvent' a_km1_ch1
         , AddQuestionEvent' a_km1_ch1_q1
         , AddQuestionEvent' a_km1_ch1_q2
         , AddAnswerEvent' a_km1_ch1_q2_aNo1
         , AddAnswerEvent' a_km1_ch1_q2_aYes1
         , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1
         , AddAnswerEvent' a_km1_ch1_q2_aNo3
         , AddAnswerEvent' a_km1_ch1_q2_aYes3
         , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
         , AddAnswerEvent' a_km1_ch1_q2_aNo4
         , AddAnswerEvent' a_km1_ch1_q2_aYes4
         , AddExpertEvent' a_km1_ch1_q2_eDarth
         , AddExpertEvent' a_km1_ch1_q2_eLuke
         , AddReferenceEvent' a_km1_ch1_q2_rCh1
         , AddReferenceEvent' a_km1_ch1_q2_rCh2
         , AddChapterEvent' a_km1_ch2
         , AddQuestionEvent' a_km1_ch2_q3
         , AddAnswerEvent' a_km1_ch2_q3_aNo2
         , AddAnswerEvent' a_km1_ch2_q3_aYes2
         ]
   let computed = migrate undefined events
   let expected = km1
   computed `shouldBe` expected
