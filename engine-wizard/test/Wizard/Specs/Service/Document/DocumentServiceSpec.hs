module Wizard.Specs.Service.Document.DocumentServiceSpec where

import Control.Lens ((&), (.~))
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import LensesConfig
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.Report.Data.Reports
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
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
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
        -- WHEN:
      (Right result) <- runInContext (createDocumentContext doc1) appContext
        -- THEN:
      compareDocumentContexts result expectation
    it "Successfully created (when levels are disabled)" $
        -- GIVEN: Prepare expectation
     do
      let expectation =
            (dmp1 & phaseUuid .~ U.nil) & report . chapterReports .~
            [report1_ch1_full_disabled_levels, report1_ch2_full_disabled_levels, report1_ch3_full_disabled_levels]
         -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
         -- AND: Prepare AppContext
      runInContext (modifyAppConfig (questionnaire . phases . enabled) False) appContext
        -- WHEN:
      (Right result) <- runInContext (createDocumentContext doc1) appContext
        -- THEN:
      compareDocumentContexts result expectation
