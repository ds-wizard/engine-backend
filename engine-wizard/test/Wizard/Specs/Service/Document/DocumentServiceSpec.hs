module Wizard.Specs.Service.Document.DocumentServiceSpec where

import Control.Lens ((&), (.~))
import Test.Hspec hiding (shouldBe)

import LensesConfig
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.Level.LevelMigration as LVL
import qualified Wizard.Database.Migration.Development.Metric.MetricMigration as MTR
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Service.Document.DocumentContextService

import Wizard.Specs.Common
import Wizard.Specs.Service.Document.Common

documentIntegrationSpec appContext =
  describe "Document Service Integration" $ describe "createDocumentContext" $ do
    it "Successfully created" $
        -- GIVEN: Prepare expectation
     do
      let expectation = dmp1
         -- AND: Run migrations
      runInContextIO QTN.runMigration appContext
      runInContextIO MTR.runMigration appContext
      runInContextIO LVL.runMigration appContext
        -- WHEN:
      (Right result) <- runInContext (createDocumentContext doc1) appContext
        -- THEN:
      compareDocumentContextDTOs result expectation
    it "Successfully created (when levels are disabled)" $
        -- GIVEN: Prepare expectation
     do
      let expectation =
            (dmp1 & level .~ 9999) & report . chapterReports .~
            [report1_ch1_full_disabled_levels, report1_ch2_full_disabled_levels, report1_ch3_full_disabled_levels]
         -- AND: Run migrations
      runInContextIO QTN.runMigration appContext
      runInContextIO MTR.runMigration appContext
      runInContextIO LVL.runMigration appContext
         -- AND: Prepare AppContext
      runInContext (modifyAppConfig (questionnaire . levels . enabled) False) appContext
        -- WHEN:
      (Right result) <- runInContext (createDocumentContext doc1) appContext
        -- THEN:
      compareDocumentContextDTOs result expectation
