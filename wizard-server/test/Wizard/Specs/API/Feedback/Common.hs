module Wizard.Specs.API.Feedback.Common where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Hspec

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService

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
  liftIO $ resDto.knowledgeModelPackageId `shouldBe` expDto.knowledgeModelPackageId
  liftIO $ resDto.title `shouldBe` expDto.title
  liftIO $ resDto.content `shouldBe` expDto.content

-- --------------------------------
-- HELPER
-- --------------------------------
loadFeedbackTokenFromEnv = do
  tcProject <- getCurrentTenantConfigProject
  updatedTcProject <- applyEnvVariable "FEEDBACK_TOKEN" tcProject.feedback.token (\t -> tcProject {feedback = tcProject.feedback {token = t}})
  modifyTenantConfigProject updatedTcProject

applyEnvVariable :: String -> String -> (String -> TenantConfigProject) -> AppContextM TenantConfigProject
applyEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  let newValue = fromMaybe oldValue envVariable
  return $ updateFn newValue
