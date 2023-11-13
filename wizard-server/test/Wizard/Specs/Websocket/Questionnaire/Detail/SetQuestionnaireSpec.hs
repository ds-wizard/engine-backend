module Wizard.Specs.Websocket.Questionnaire.Detail.SetQuestionnaireSpec where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (ok200)
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Model.Http.HttpRequest
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Questionnaire.Detail.Common

setQuestionnaireSpec appContext =
  describe "setQuestionnaireSpec" $
    test200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 appContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      let qtn = questionnaire10
      runInContext U.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      runInContextIO (insertQuestionnaire questionnaire7) appContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext questionnaire10.uuid
      ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers appContext questionnaire7.uuid
      -- AND: Prepare request
      let request =
            HttpRequest
              { requestMethod = "PUT"
              , requestUrl = "/wizard-api/questionnaires/" ++ U.toString questionnaire10.uuid
              , requestHeaders = M.fromList [("Authorization", "Bearer " ++ reqAuthToken), ("Content-Type", "application/json")]
              , requestBody = BSL.toStrict . encode $ questionnaire10EditedChange
              , multipart = Nothing
              }
      -- AND: Prepare expectation
      let expStatus = 200
      -- WHEN:
      response <- runSimpleRequest appContext request
      -- THEN:
      isRight response `shouldBe` True
      let (Right r) = response
      HC.responseStatus r `shouldBe` ok200
      -- AND:
      read_SetQuestionnaire c1 questionnaire10EditedWs
      read_SetQuestionnaire c2 questionnaire10EditedWs
      read_SetQuestionnaire c3 questionnaire10EditedWs
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
read_SetQuestionnaire connection expQtnChangeDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerQuestionnaireActionDTO)
  let (Right (Success_ServerActionDTO (SetQuestionnaire_ServerQuestionnaireActionDTO qtnChangeDto))) = eResult
  expQtnChangeDto `shouldBe` qtnChangeDto
