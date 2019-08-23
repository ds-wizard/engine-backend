module Specs.Service.DataManagementPlan.Common where

import Control.Lens ((^.))
import Test.Hspec.Expectations.Pretty

import LensesConfig

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDataManagementPlanDtos resDto expDto = do
  (resDto ^. questionnaireUuid) `shouldBe` (expDto ^. questionnaireUuid)
  (resDto ^. questionnaireName) `shouldBe` (expDto ^. questionnaireName)
  (resDto ^. questionnaireReplies) `shouldBe` (expDto ^. questionnaireReplies)
  (resDto ^. level) `shouldBe` (expDto ^. level)
  (resDto ^. knowledgeModel) `shouldBe` (expDto ^. knowledgeModel)
  (resDto ^. metrics) `shouldBe` (expDto ^. metrics)
  (resDto ^. levels) `shouldBe` (expDto ^. levels)
  (resDto ^. report . chapterReports) `shouldBe` (expDto ^. report . chapterReports)
  (resDto ^. package) `shouldBe` (expDto ^. package)
  (resDto ^. organization) `shouldBe` (expDto ^. organization)
  (resDto ^. createdBy) `shouldBe` (expDto ^. createdBy)
