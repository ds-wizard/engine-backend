module Wizard.Specs.Service.Report.ReportGeneratorSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Model.Project.ProjectContent
import Wizard.Service.Report.ReportGenerator

reportGeneratorSpec =
  describe "Report Generator" $ do
    describe "computeChapterReport" $ do
      createComputeChapterReportTest 1 chapter1 report1_ch1_full
      createComputeChapterReportTest 2 chapter2 report1_ch2_full
      createComputeChapterReportTest 3 chapter3 report1_ch3_full
    describe "computeTotalReport" $ createComputeTotalReportTest 3 report1_total_full

createComputeChapterReportTest number chapter expectation =
  it ("computeChapterReport for chapter" ++ show number ++ " should work") $
    -- GIVEN: Prepare
    do
      let requiredPhaseUuidUuid = Just $ phase1.uuid
      let km = km1WithQ4
      let rs = M.fromList $ unused_rQ2_aYes_fuQ1_aYes_fuq2 : M.toList project1Ctn.replies
      -- WHEN:
      let result = computeChapterReport requiredPhaseUuidUuid km rs chapter
      -- THEN
      result `shouldBe` expectation

createComputeTotalReportTest number expectation =
  it "computeTotalReport should work" $
    -- GIVEN: Prepare
    do
      let requiredPhaseUuidUuid = Just $ phase1.uuid
      let km = km1WithQ4
      let rs = M.fromList $ unused_rQ2_aYes_fuQ1_aYes_fuq2 : M.toList project1Ctn.replies
      -- WHEN:
      let result = computeTotalReport requiredPhaseUuidUuid km rs
      -- THEN
      result `shouldBe` expectation
