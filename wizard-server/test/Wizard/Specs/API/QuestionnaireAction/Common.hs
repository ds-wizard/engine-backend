module Wizard.Specs.API.QuestionnaireAction.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.QuestionnaireAction.QuestionnaireActionDAO
import Wizard.Model.QuestionnaireAction.QuestionnaireAction

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireActionInDB appContext action = do
  eQuestionnaireAction <- runInContextIO (findQuestionnaireActionById action.qaId) appContext
  liftIO $ isRight eQuestionnaireAction `shouldBe` True
  let (Right actionFromDB) = eQuestionnaireAction
  compareQuestionnaireActionDtos actionFromDB action

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireActionDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
