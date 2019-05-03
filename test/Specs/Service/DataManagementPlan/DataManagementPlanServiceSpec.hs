module Specs.Service.DataManagementPlan.DataManagementPlanServiceSpec where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.DataManagementPlan.Data.DataManagementPlans
import Database.Migration.Development.FilledKnowledgeModel.Data.FilledKnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import qualified
       Database.Migration.Development.Level.LevelMigration as LVL
import qualified
       Database.Migration.Development.Metric.MetricMigration as MTR
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import LensesConfig
import Service.DataManagementPlan.DataManagementPlanMapper
import Service.DataManagementPlan.DataManagementPlanService

import Specs.Common
import Specs.Service.DataManagementPlan.Common

dataManagementPlanSpec =
  describe "DataManagementPlan Service" $
  it "createFilledKM" $ createFilledKM km1WithQ4 (questionnaire1 ^. replies) `shouldBe` fKm1WithQ4

dataManagementPlanIntegrationSpec appContext =
  describe "DataManagementPlan Service Integration" $
  describe "createDataManagementPlan" $ do
    it "Successfully created" $
        -- GIVEN: Prepare expectation
     do
      let expectation = toDTO dmp1
         -- AND: Run migrations
      runInContextIO QTN.runMigration appContext
      runInContextIO MTR.runMigration appContext
      runInContextIO LVL.runMigration appContext
        -- WHEN:
      (Right result) <- runInContext (createDataManagementPlan . U.toString $ questionnaire1 ^. uuid) appContext
        -- THEN:
      compareDataManagementPlanDtos result expectation
    it "Successfully created (when levels are disabled)" $
        -- GIVEN: Prepare expectation
     do
      let expectation = toDTO (dmp1 & level .~ 9999)
         -- AND: Run migrations
      runInContextIO QTN.runMigration appContext
      runInContextIO MTR.runMigration appContext
      runInContextIO LVL.runMigration appContext
         -- AND: Prepare AppContext
      let updatedAppContext = appContext & (appConfig . general . levelsEnabled) .~ False
        -- WHEN:
      (Right result) <- runInContext (createDataManagementPlan . U.toString $ questionnaire1 ^. uuid) updatedAppContext
        -- THEN:
      compareDataManagementPlanDtos result expectation
