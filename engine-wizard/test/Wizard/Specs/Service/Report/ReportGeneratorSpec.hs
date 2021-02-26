module Wizard.Specs.Service.Report.ReportGeneratorSpec where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Test.Hspec

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Service.Report.ReportGenerator

reportGeneratorSpec =
  describe "Report Generator" $ do
    describe "computeChapterReport" $ do
      createComputeChapterReportTest False 1 chapter1 report1_ch1_full_disabled_levels
      createComputeChapterReportTest False 2 chapter2 report1_ch2_full_disabled_levels
      createComputeChapterReportTest False 3 chapter3 report1_ch3_full_disabled_levels
      createComputeChapterReportTest True 1 chapter1 report1_ch1_full
      createComputeChapterReportTest True 2 chapter2 report1_ch2_full
      createComputeChapterReportTest True 3 chapter3 report1_ch3_full
    describe "computeTotalReport" $ do
      createComputeTotalReportTest True 3 report1_total_full
      createComputeTotalReportTest False 3 report1_total_full_disabled_levels

createComputeChapterReportTest levelsEnabled number chapter expectation =
  it ("computeChapterReport for chapter" ++ show number ++ " should work (levelsEnabled: " ++ show levelsEnabled ++ ")") $
    -- GIVEN: Prepare
   do
    let requiredLevel = 1
    let metrics = [metricF, metricA, metricI, metricR, metricG, metricO]
    let km = km1WithQ4
    let rs = unused_rQ2_aYes_fuQ1_aYes_fuq2 : M.toList (questionnaire1Ctn ^. replies)
    -- WHEN:
    let result = computeChapterReport levelsEnabled requiredLevel metrics km rs chapter
    -- THEN
    result `shouldBe` expectation

createComputeTotalReportTest levelsEnabled number expectation =
  it ("computeTotalReport should work (levelsEnabled: " ++ show levelsEnabled ++ ")") $
    -- GIVEN: Prepare
   do
    let requiredLevel = 1
    let metrics = [metricF, metricA, metricI, metricR, metricG, metricO]
    let km = km1WithQ4
    let rs = unused_rQ2_aYes_fuQ1_aYes_fuq2 : M.toList (questionnaire1Ctn ^. replies)
    -- WHEN:
    let result = computeTotalReport levelsEnabled requiredLevel metrics km rs
    -- THEN
    result `shouldBe` expectation
