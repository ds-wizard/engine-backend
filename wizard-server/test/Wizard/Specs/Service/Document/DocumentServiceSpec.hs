module Wizard.Specs.Service.Document.DocumentServiceSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as USR
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Report.Report
import Wizard.Service.Document.Context.DocumentContextService
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SP_Mapper

import Wizard.Specs.Common
import Wizard.Specs.Service.Document.Common

documentIntegrationSpec appContext =
  describe "Document Service Integration" $
    describe "createDocumentContext" $
      it "Successfully created" $
        -- GIVEN: Prepare expectation
        do
          let expectation = dmp1
          -- AND: Run migrations
          runInContextIO USR.runMigration appContext
          runInContextIO TML.runMigration appContext
          runInContextIO QTN.runMigration appContext
          -- WHEN:
          (Right result) <- runInContext (createDocumentContext doc1 (SP_Mapper.toPackage germanyPackage) [] questionnaire1 Nothing) appContext
          -- THEN:
          compareDocumentContexts result expectation
