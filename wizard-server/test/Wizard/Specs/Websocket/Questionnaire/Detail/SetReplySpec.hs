module Wizard.Specs.Websocket.Questionnaire.Detail.SetReplySpec where

import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Questionnaire.Detail.Common

setReplySpec appContext =
  describe "setReply" $ do
    test200 appContext
    test403 appContext

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
      write_SetReply c1 (toEventChangeDTO (sre_rQ1Updated' questionnaire10Uuid))
      -- THEN:
      read_SetReply c1 (toEventDTO (sre_rQ1Updated' questionnaire10Uuid) (Just userAlbert))
      read_SetReply c2 (toEventDTO (sre_rQ1Updated' questionnaire10Uuid) (Just userAlbert))
      read_SetReply c3 (toEventDTO (sre_rQ1Updated' questionnaire10Uuid) (Just userAlbert))
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test403 appContext = do
  create_403_no_perm
    "WS 403 FORBIDDEN - no required edit entity permission (Anonymous, VisibleView)"
    appContext
    questionnaire5
    Nothing
    "Edit Questionnaire"
  create_403_no_perm
    "WS 403 FORBIDDEN - no required edit entity permission (Non-owner, VisibleView)"
    appContext
    questionnaire5
    (Just reqNonAdminAuthToken)
    "Edit Questionnaire"

create_403_no_perm title appContext qtn authToken errorMessage =
  it title $
    -- GIVEN: Prepare database
    do
      insertQuestionnaireAndUsers appContext qtn
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN errorMessage
      -- AND: Connect to websocket
      (c1, s1) <- createConnection appContext (reqUrlT qtn.uuid authToken)
      read_SetUserList c1 0
      -- WHEN: Send setReply
      write_SetReply c1 (toEventChangeDTO (sre_rQ1Updated' qtn.uuid))
      -- THEN: Read response
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_SetReply connection replyDto = do
  let reqDto = SetContent_ClientQuestionnaireActionDTO replyDto
  sendMessage connection reqDto

read_SetReply connection expReplyDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerQuestionnaireActionDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerQuestionnaireActionDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
