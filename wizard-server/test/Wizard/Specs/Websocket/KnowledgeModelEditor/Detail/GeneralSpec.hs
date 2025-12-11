module Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.GeneralSpec where

import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Localization.Messages.Public
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Service.KnowledgeModel.Editor.EditorService

import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.Common

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
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers appContext editor
      -- WHEN
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext editor.uuid
      -- THEN:
      assertCountOfWebsocketConnection appContext 2
      -- AND: Close sockets
      closeSockets [s1, s2]

---- ----------------------------------------------------
---- ----------------------------------------------------
---- ----------------------------------------------------
test403 appContext =
  it "WS 403 FORBIDDEN" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers appContext editor
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Missing permission: KM_PERM"
      -- WHEN: Connect to websocket
      (c1, s1) <- createConnection appContext (reqUrlT editor.uuid (Just reqIsaacAuthToken))
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
      let nonExistingEditorUuid = "fd5ea37c-852a-4174-9d65-2bf23202541d"
      -- AND: Prepare expectation
      let expError =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "knowledge_model_editor"
                  [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", nonExistingEditorUuid)]
              )
      -- WHEN:
      (c1, s1) <- createConnection appContext (reqUrlT (u' nonExistingEditorUuid) (Just reqAuthToken))
      -- THEN:
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]
  it "WS 404 NOT FOUND - project was deleted" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers appContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext editor.uuid
      -- AND: Prepare expectation
      let expError = NotExistsError (_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__COLLABORATION__FORCE_DISCONNECT (U.toString $ editor.uuid))
      -- WHEN: Update permission
      runInContext (deleteEditor editor.uuid) appContext
      -- THEN: Read response
      read_Error c1 expError
      read_Error c2 expError
      -- AND: Close sockets
      closeSockets [s1, s2]
