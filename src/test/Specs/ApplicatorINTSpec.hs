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
import KMMigration.Migration.Event.Chapter.AddChapterEvent
import KMMigration.Migration.Event.Common
import KMMigration.Migration.Event.EventApplicable
import KMMigration.Migration.Event.KnowledgeModel.EditKnowledgeModelEvent
import KMMigration.Migration.Migration
import KMMigration.Model.Event
import KMMigration.Model.KnowledgeModel

applicatorINTSpec =
  describe "Applicator Integration test" $
  it "Apply: Create KM from scratch" $ do
    let events =
          [ MkEventApplicable a_km1
          , MkEventApplicable a_km1_ch1
          , MkEventApplicable a_km1_ch1_q1
          , MkEventApplicable a_km1_ch1_q2
          , MkEventApplicable a_km1_ch1_q2_aNo1
          , MkEventApplicable a_km1_ch1_q2_aYes1
          , MkEventApplicable a_km1_ch1_ansYes1_fuq1
          , MkEventApplicable a_km1_ch1_q2_aNo3
          , MkEventApplicable a_km1_ch1_q2_aYes3
          , MkEventApplicable a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
          , MkEventApplicable a_km1_ch1_q2_aNo4
          , MkEventApplicable a_km1_ch1_q2_aYes4
          , MkEventApplicable a_km1_ch1_q2_eDarth
          , MkEventApplicable a_km1_ch1_q2_eLuke
          , MkEventApplicable a_km1_ch1_q2_rCh1
          , MkEventApplicable a_km1_ch1_q2_rCh2
          , MkEventApplicable a_km1_ch2
          , MkEventApplicable a_km1_ch2_q3
          , MkEventApplicable a_km1_ch2_q3_aNo2
          , MkEventApplicable a_km1_ch2_q3_aYes2
          ]
    let computed = migrate undefined events
    let expected = km1
    computed `shouldBe` expected
