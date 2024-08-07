module Wizard.Specs.Service.Document.Common where

import Test.Hspec.Expectations.Pretty

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentContexts resDto expDto = do
  resDto.document.uuid `shouldBe` expDto.document.uuid
  resDto.document.name `shouldBe` expDto.document.name
  resDto.questionnaire.uuid `shouldBe` expDto.questionnaire.uuid
  resDto.questionnaire.name `shouldBe` expDto.questionnaire.name
  resDto.questionnaire.replies `shouldBe` expDto.questionnaire.replies
  resDto.questionnaire.phaseUuid `shouldBe` expDto.questionnaire.phaseUuid
  resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  resDto.report.chapterReports `shouldBe` expDto.report.chapterReports
  resDto.package `shouldBe` expDto.package
  resDto.organization `shouldBe` expDto.organization
  resDto.users `shouldBe` expDto.users
  resDto.groups `shouldBe` expDto.groups
