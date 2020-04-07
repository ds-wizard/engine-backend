module Wizard.Specs.API.Feedback.Common where

import Control.Lens (Lens', (&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Hspec

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Config.AppConfigService

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
  liftIO $ (resDto ^. questionUuid) `shouldBe` (expDto ^. questionUuid)
  liftIO $ (resDto ^. packageId) `shouldBe` (expDto ^. packageId)
  liftIO $ (resDto ^. title) `shouldBe` (expDto ^. title)
  liftIO $ (resDto ^. content) `shouldBe` (expDto ^. content)

-- --------------------------------
-- HELPER
-- --------------------------------
loadFeedbackTokenFromEnv = do
  appConfig <- getAppConfig
  updatedAppConfig <- applyEnvVariable "FEEDBACK_TOKEN" (questionnaire . feedback . token) appConfig
  modifyAppConfig (toChangeDTO updatedAppConfig)

applyEnvVariable :: String -> Lens' AppConfig String -> AppConfig -> AppContextM AppConfig
applyEnvVariable envVariableName accessor config = do
  envVariable <- liftIO $ lookupEnv envVariableName
  let newValue = fromMaybe (config ^. accessor) envVariable
  return $ config & accessor .~ newValue
