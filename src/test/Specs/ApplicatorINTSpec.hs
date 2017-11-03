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
import KMMigration.Migration.Event.Common
import Model.Event.Event
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import KMMigration.Migration.Migration
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

applicatorINTSpec =
 describe "Applicator Integration test" $
 it "Apply: Create KM from scratch" $ do
   let events =
         [ MkEvent a_km1
         , MkEvent a_km1_ch1
         , MkEvent a_km1_ch1_q1
         , MkEvent a_km1_ch1_q2
         , MkEvent a_km1_ch1_q2_aNo1
         , MkEvent a_km1_ch1_q2_aYes1
         , MkEvent a_km1_ch1_ansYes1_fuq1
         , MkEvent a_km1_ch1_q2_aNo3
         , MkEvent a_km1_ch1_q2_aYes3
         , MkEvent a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
         , MkEvent a_km1_ch1_q2_aNo4
         , MkEvent a_km1_ch1_q2_aYes4
         , MkEvent a_km1_ch1_q2_eDarth
         , MkEvent a_km1_ch1_q2_eLuke
         , MkEvent a_km1_ch1_q2_rCh1
         , MkEvent a_km1_ch1_q2_rCh2
         , MkEvent a_km1_ch2
         , MkEvent a_km1_ch2_q3
         , MkEvent a_km1_ch2_q3_aNo2
         , MkEvent a_km1_ch2_q3_aYes2
         ]
   let computed = migrate undefined events
   let expected = km1
   computed `shouldBe` expected
