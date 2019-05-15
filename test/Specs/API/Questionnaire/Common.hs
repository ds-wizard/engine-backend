module Specs.API.Questionnaire.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireInDB appContext qtn = do
  eQtn <- runInContextIO (findQuestionnaireById (U.toString $ qtn ^. uuid)) appContext
  liftIO $ (isRight eQtn) `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireDtos qtnFromDb qtn

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireCreateDtos resDto expDto = do
  liftIO $ resDto ^. name `shouldBe` expDto ^. name
  liftIO $ resDto ^. level `shouldBe` expDto ^. level
  liftIO $ resDto ^. accessibility `shouldBe` expDto ^. accessibility
  liftIO $ resDto ^. package `shouldBe` expDto ^. package
  liftIO $ resDto ^. ownerUuid `shouldBe` expDto ^. ownerUuid

compareQuestionnaireDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True

compareReportDtos resDto expDto = liftIO $ (resDto ^. chapterReports == expDto ^. chapterReports) `shouldBe` True
