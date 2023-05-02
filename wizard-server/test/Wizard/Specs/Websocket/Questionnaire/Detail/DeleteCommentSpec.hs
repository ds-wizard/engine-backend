module Wizard.Specs.Websocket.Questionnaire.Detail.DeleteCommentSpec where

import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Questionnaire.Detail.Common

deleteCommentSpec appContext = describe "deleteComment" $ test200 appContext

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
      -- WHEN:
      write_DeleteComment c1 dcche_rQ1_t1_1'
      -- THEN:
      read_DeleteComment c1 dce_rQ1_t1_1'
      read_DeleteComment c2 dce_rQ1_t1_1'
      read_DeleteComment c3 dce_rQ1_t1_1'
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_DeleteComment connection replyDto = do
  let reqDto = SetContent_ClientQuestionnaireActionDTO replyDto
  sendMessage connection reqDto

read_DeleteComment connection expReplyDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerQuestionnaireActionDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerQuestionnaireActionDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
