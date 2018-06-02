module Specs.Service.DataManagementPlan.DataManagementPlanServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.FilledKnowledgeModel.Data.FilledKnowledgeModels
import Database.Migration.Questionnaire.Data.Questionnaires
import Service.DataManagementPlan.DataManagementPlanService

dataManagementPlanSpec =
  describe "DataManagementPlan Service succeeds" $
  it "createFilledKM" $ do createFilledKM questionnaire1 `shouldBe` fKm1WithQ4
