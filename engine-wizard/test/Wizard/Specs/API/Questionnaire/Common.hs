module Wizard.Specs.API.Questionnaire.Common where

import Control.Lens ((^.))
import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireInDB appContext qtn = do
  eQtn <- runInContextIO (findQuestionnaireById (U.toString $ qtn ^. uuid)) appContext
  liftIO $ isRight eQtn `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireDtos qtnFromDb qtn

assertExistenceOfQuestionnaireContentInDB appContext qtnUuid content = do
  eQtn <- runInContextIO (findQuestionnaireById (U.toString qtnUuid)) appContext
  liftIO $ isRight eQtn `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireContentDtos qtnFromDb content

assertAbsenceOfQuestionnaireInDB appContext qtn = do
  let qtnUuid = U.toString $ qtn ^. uuid
  eQtn <- runInContextIO (findQuestionnaireById qtnUuid) appContext
  liftIO $ isLeft eQtn `shouldBe` True
  let (Left error) = eQtn
  liftIO $ error `shouldBe` (NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire" qtnUuid)

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireCreateDtos resDto expDto = do
  liftIO $ resDto ^. name `shouldBe` expDto ^. name
  liftIO $ resDto ^. level `shouldBe` expDto ^. level
  liftIO $ resDto ^. visibility `shouldBe` expDto ^. visibility
  liftIO $ resDto ^. sharing `shouldBe` expDto ^. sharing
  liftIO $ resDto ^. package `shouldBe` expDto ^. package

compareQuestionnaireCloneDtos resDto expDto = do
  liftIO $ resDto ^. uuid `shouldNotBe` expDto ^. uuid
  liftIO $ resDto ^. name `shouldBe` ("Copy of " ++ expDto ^. name)
  liftIO $ resDto ^. level `shouldBe` expDto ^. level
  liftIO $ resDto ^. visibility `shouldBe` expDto ^. visibility
  liftIO $ resDto ^. sharing `shouldBe` expDto ^. sharing
  liftIO $ resDto ^. state `shouldBe` expDto ^. state
  liftIO $ resDto ^. package `shouldBe` expDto ^. package

compareQuestionnaireCreateDtos' resDto expDto = do
  liftIO $ resDto ^. name `shouldBe` expDto ^. name
  liftIO $ resDto ^. level `shouldBe` expDto ^. level
  liftIO $ resDto ^. visibility `shouldBe` expDto ^. visibility
  liftIO $ resDto ^. sharing `shouldBe` expDto ^. sharing
  liftIO $ resDto ^. state `shouldBe` expDto ^. state
  liftIO $ resDto ^. package `shouldBe` expDto ^. package
  liftIO $ resDto ^. selectedTagUuids `shouldBe` expDto ^. selectedTagUuids
  liftIO $ resDto ^. knowledgeModel `shouldBe` expDto ^. knowledgeModel
  liftIO $ resDto ^. replies `shouldBe` expDto ^. replies

compareQuestionnaireDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True

compareQuestionnaireContentDtos resDto expDto = liftIO $ (resDto ^. events) == (expDto ^. events) `shouldBe` True

compareReportDtos resDto expDto = do
  liftIO $ resDto ^. totalReport `shouldBe` expDto ^. totalReport
  liftIO $ resDto ^. chapterReports `shouldBe` expDto ^. chapterReports
