module Wizard.Specs.Service.Document.Common where

import Test.Hspec.Expectations.Pretty

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentContexts resDto expDto = do
  resDto.document.uuid `shouldBe` expDto.document.uuid
  resDto.document.name `shouldBe` expDto.document.name
  resDto.project.uuid `shouldBe` expDto.project.uuid
  resDto.project.name `shouldBe` expDto.project.name
  resDto.project.replies `shouldBe` expDto.project.replies
  resDto.project.phaseUuid `shouldBe` expDto.project.phaseUuid
  resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  resDto.report.chapterReports `shouldBe` expDto.report.chapterReports
  resDto.package `shouldBe` expDto.package
  resDto.organization `shouldBe` expDto.organization
  resDto.users `shouldBe` expDto.users
  resDto.groups `shouldBe` expDto.groups
