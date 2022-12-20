module Wizard.Specs.API.Feedback.Common where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Hspec

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.App.AppConfigMapper
import Wizard.Service.Config.App.AppConfigService

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfFeedbackInDB appContext feedback = do
  feedbackFromDb <- getFirstFromDB findFeedbacks appContext
  compareFeedbackDtos feedbackFromDb feedback

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareFeedbackDtos resDto expDto = do
  liftIO $ resDto.questionUuid `shouldBe` expDto.questionUuid
  liftIO $ resDto.packageId `shouldBe` expDto.packageId
  liftIO $ resDto.title `shouldBe` expDto.title
  liftIO $ resDto.content `shouldBe` expDto.content

-- --------------------------------
-- HELPER
-- --------------------------------
loadFeedbackTokenFromEnv = do
  appConfig <- getAppConfig
  updatedAppConfig <- applyEnvVariable "FEEDBACK_TOKEN" appConfig.questionnaire.feedback.token (\t -> appConfig {questionnaire = appConfig.questionnaire {feedback = appConfig.questionnaire.feedback {token = t}}})
  modifyAppConfigDto (toChangeDTO updatedAppConfig)

applyEnvVariable :: String -> String -> (String -> AppConfig) -> AppContextM AppConfig
applyEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  let newValue = fromMaybe oldValue envVariable
  return $ updateFn newValue
