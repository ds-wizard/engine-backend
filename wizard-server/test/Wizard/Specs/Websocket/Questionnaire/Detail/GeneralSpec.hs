module Wizard.Specs.Websocket.Questionnaire.Detail.GeneralSpec where

import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Localization.Messages.Public
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireService

import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Questionnaire.Detail.Common

generalSpec appContext =
  describe "general" $ do
    test200 appContext
    test403 appContext
    test404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 appContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      let qtn = questionnaire10
      insertQuestionnaireAndUsers appContext qtn
      -- WHEN
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext qtn.uuid
      -- THEN:
      assertCountOfWebsocketConnection appContext 3
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test403 appContext = do
  create_403_no_perm
    "WS 403 FORBIDDEN - no required view entity permission (Anonymous, Private)"
    appContext
    questionnaire1
    Nothing
    "View Questionnaire"
  create_403_no_perm
    "WS 403 FORBIDDEN - no required view entity permission (Non-owner, Private)"
    appContext
    questionnaire1
    (Just reqNonAdminAuthToken)
    "View Questionnaire"
  it "WS 403 FORBIDDEN - when perms are changed" $
    -- GIVEN: Prepare database
    do
      let qtn = questionnaire10
      insertQuestionnaireAndUsers appContext qtn
      let updatedQtn = qtn {visibility = PrivateQuestionnaire, sharing = RestrictedQuestionnaire}
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext qtn.uuid
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"
      -- WHEN: Update permission
      runInContext (modifyQuestionnaire updatedQtn.uuid (toChangeDTO updatedQtn)) appContext
      -- THEN: Read response
      read_SetUserList c1 1
      read_SetUserList c1 0
      read_SetUserList_or_Error c2 expError
      read_SetUserList_or_Error c3 expError
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

create_403_no_perm title appContext qtn authToken errorMessage =
  it title $
    -- GIVEN: Prepare database
    do
      insertQuestionnaireAndUsers appContext qtn
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN errorMessage
      -- WHEN: Connect to websocket
      (c1, s1) <- createConnection appContext (reqUrlT qtn.uuid authToken)
      -- THEN: Read response
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test404 appContext = do
  it "WS 404 NOT FOUND - non existing entity" $
    -- GIVEN: Prepare request
    do
      let nonExistingQtnUuid = "fd5ea37c-852a-4174-9d65-2bf23202541d"
      -- AND: Prepare expectation
      let expError =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "questionnaire"
                  [("app_uuid", U.toString defaultApp.uuid), ("uuid", nonExistingQtnUuid)]
              )
      -- WHEN:
      (c1, s1) <- createConnection appContext (reqUrlT (u' nonExistingQtnUuid) (Just reqAuthToken))
      -- THEN:
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]
  it "WS 404 NOT FOUND - questionnaire was deleted" $
    -- GIVEN: Prepare database
    do
      let qtn = questionnaire10
      insertQuestionnaireAndUsers appContext qtn
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext qtn.uuid
      -- AND: Prepare expectation
      let expError = NotExistsError (_ERROR_SERVICE_QTN_COLLABORATION__FORCE_DISCONNECT (U.toString $ qtn.uuid))
      -- WHEN: Update permission
      runInContext (deleteQuestionnaire qtn.uuid True) appContext
      -- THEN: Read response
      read_Error c1 expError
      read_Error c2 expError
      read_Error c3 expError
      -- AND: Close sockets
      closeSockets [s1, s2, s3]
