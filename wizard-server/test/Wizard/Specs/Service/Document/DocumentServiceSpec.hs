module Wizard.Specs.Service.Document.DocumentServiceSpec where

import Data.Foldable (traverse_)
import Test.Hspec hiding (shouldBe)

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as USR
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Report.Report
import Wizard.Service.Document.Context.DocumentContextService

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
          runInContextIO PRJ.runMigration appContext
          runInContextIO (insertPackage germanyKmPackage) appContext
          runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
          -- WHEN:
          (Right result) <- runInContext (createDocumentContext doc1 germanyKmPackage [] project1 Nothing) appContext
          -- THEN:
          compareDocumentContexts result expectation
