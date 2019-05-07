module Specs.Service.DataManagementPlan.DataManagementPlanServiceSpec where

import Control.Lens ((^.))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledKnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import LensesConfig
import Service.DataManagementPlan.DataManagementPlanService

dataManagementPlanSpec =
  describe "DataManagementPlan Service succeeds" $
  it "createFilledKM" $ do createFilledKM km1WithQ4 (questionnaire1 ^. replies) `shouldBe` fKm1WithQ4
