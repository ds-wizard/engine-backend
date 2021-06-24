module Wizard.Specs.Service.Document.Common where

import Control.Lens ((^.))
import Test.Hspec.Expectations.Pretty

import LensesConfig

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentContexts resDto expDto = do
  (resDto ^. questionnaireUuid) `shouldBe` (expDto ^. questionnaireUuid)
  (resDto ^. questionnaireName) `shouldBe` (expDto ^. questionnaireName)
  (resDto ^. questionnaireReplies) `shouldBe` (expDto ^. questionnaireReplies)
  (resDto ^. phaseUuid) `shouldBe` (expDto ^. phaseUuid)
  (resDto ^. knowledgeModel) `shouldBe` (expDto ^. knowledgeModel)
  (resDto ^. report . chapterReports) `shouldBe` (expDto ^. report . chapterReports)
  (resDto ^. package) `shouldBe` (expDto ^. package)
  (resDto ^. organization) `shouldBe` (expDto ^. organization)
  (resDto ^. createdBy) `shouldBe` (expDto ^. createdBy)
