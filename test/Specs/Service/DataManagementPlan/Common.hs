module Specs.Service.DataManagementPlan.Common where

import Control.Lens ((^.))
import Test.Hspec.Expectations.Pretty

import LensesConfig

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDataManagementPlanDtos resDto expDto = do
  (resDto ^. questionnaireUuid) `shouldBe` (expDto ^. questionnaireUuid)
  (resDto ^. level) `shouldBe` (expDto ^. level)
  (resDto ^. filledKnowledgeModel) `shouldBe` (expDto ^. filledKnowledgeModel)
  (resDto ^. metrics) `shouldBe` (expDto ^. metrics)
  (resDto ^. levels) `shouldBe` (expDto ^. levels)
  (resDto ^. report . chapterReports) `shouldBe` (expDto ^. report . chapterReports)
  (resDto ^. package) `shouldBe` (expDto ^. package)
  (resDto ^. organization) `shouldBe` (expDto ^. organization)
  (resDto ^. createdBy) `shouldBe` (expDto ^. createdBy)
