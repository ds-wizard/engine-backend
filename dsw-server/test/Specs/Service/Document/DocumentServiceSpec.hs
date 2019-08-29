module Specs.Service.Document.DocumentServiceSpec where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Database.Migration.Development.Document.Data.Documents
import qualified
       Database.Migration.Development.Level.LevelMigration as LVL
import qualified
       Database.Migration.Development.Metric.MetricMigration as MTR
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import LensesConfig
import Service.Document.DocumentMapper
import Service.Document.DocumentService

import Specs.Common
import Specs.Service.Document.Common

documentIntegrationSpec appContext =
  describe "Document Service Integration" $
  describe "createDocumentContext" $ do
    it "Successfully created" $
        -- GIVEN: Prepare expectation
     do
      let expectation = toDocumentContextDTO dmp1
         -- AND: Run migrations
      runInContextIO QTN.runMigration appContext
      runInContextIO MTR.runMigration appContext
      runInContextIO LVL.runMigration appContext
        -- WHEN:
      (Right result) <- runInContext (createDocumentContext . U.toString $ questionnaire1 ^. uuid) appContext
        -- THEN:
      compareDocumentContextDTOs result expectation
    it "Successfully created (when levels are disabled)" $
        -- GIVEN: Prepare expectation
     do
      let expectation = toDocumentContextDTO (dmp1 & level .~ 9999)
         -- AND: Run migrations
      runInContextIO QTN.runMigration appContext
      runInContextIO MTR.runMigration appContext
      runInContextIO LVL.runMigration appContext
         -- AND: Prepare AppContext
      let updatedAppContext = appContext & (appConfig . general . levelsEnabled) .~ False
        -- WHEN:
      (Right result) <- runInContext (createDocumentContext . U.toString $ questionnaire1 ^. uuid) updatedAppContext
        -- THEN:
      compareDocumentContextDTOs result expectation
