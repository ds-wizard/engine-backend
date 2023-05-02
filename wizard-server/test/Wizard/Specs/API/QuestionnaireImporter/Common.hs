module Wizard.Specs.API.QuestionnaireImporter.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireImporterInDB appContext importer = do
  eQuestionnaireImporter <- runInContextIO (findQuestionnaireImporterById importer.qiId) appContext
  liftIO $ isRight eQuestionnaireImporter `shouldBe` True
  let (Right importerFromDB) = eQuestionnaireImporter
  compareQuestionnaireImporterDtos importerFromDB importer

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireImporterDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
