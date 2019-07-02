module Specs.API.Questionnaire.Common where

import Control.Lens ((^.))
import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Localization
import Model.Error.Error

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireInDB appContext qtn = do
  eQtn <- runInContextIO (findQuestionnaireById (U.toString $ qtn ^. uuid)) appContext
  liftIO $ (isRight eQtn) `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireDtos qtnFromDb qtn

assertAbsenceOfQuestionnaireInDB appContext qtn = do
  let qtnUuid = U.toString $ qtn ^. uuid
  eQtn <- runInContextIO (findQuestionnaireById qtnUuid) appContext
  liftIO $ (isLeft eQtn) `shouldBe` True
  let (Left error) = eQtn
  liftIO $ error `shouldBe` (NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire" qtnUuid)

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireCreateDtos resDto expDto = do
  liftIO $ resDto ^. name `shouldBe` expDto ^. name
  liftIO $ resDto ^. level `shouldBe` expDto ^. level
  liftIO $ resDto ^. accessibility `shouldBe` expDto ^. accessibility
  liftIO $ resDto ^. package `shouldBe` expDto ^. package
  liftIO $ resDto ^. ownerUuid `shouldBe` expDto ^. ownerUuid

compareQuestionnaireCreateDtos' resDto expDto = do
  liftIO $ resDto ^. name `shouldBe` expDto ^. name
  liftIO $ resDto ^. level `shouldBe` expDto ^. level
  liftIO $ resDto ^. accessibility `shouldBe` expDto ^. accessibility
  liftIO $ resDto ^. state `shouldBe` expDto ^. state
  liftIO $ resDto ^. package `shouldBe` expDto ^. package
  liftIO $ resDto ^. selectedTagUuids `shouldBe` expDto ^. selectedTagUuids
  liftIO $ resDto ^. knowledgeModel `shouldBe` expDto ^. knowledgeModel
  liftIO $ resDto ^. replies `shouldBe` expDto ^. replies
  liftIO $ resDto ^. ownerUuid `shouldBe` expDto ^. ownerUuid

compareQuestionnaireDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True

compareReportDtos resDto expDto = liftIO $ (resDto ^. chapterReports == expDto ^. chapterReports) `shouldBe` True
