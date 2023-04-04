module Wizard.Specs.Websocket.Branch.Detail.GeneralSpec where

import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Localization.Messages.Public
import Wizard.Model.Branch.Branch
import Wizard.Service.Branch.BranchService

import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Branch.Detail.Common
import Wizard.Specs.Websocket.Common

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
      let branch = amsterdamBranch
      let branchData = amsterdamBranchData
      insertBranchAndUsers appContext branch branchData
      -- WHEN
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext branch.uuid
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
      let branch = amsterdamBranch
      let branchData = amsterdamBranchData
      insertBranchAndUsers appContext branch branchData
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Missing permission: KM_PERM"
      -- WHEN: Connect to websocket
      (c1, s1) <- createConnection appContext (reqUrlT branch.uuid (Just reqIsaacAuthToken))
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
      let nonExistingBranchUuid = "fd5ea37c-852a-4174-9d65-2bf23202541d"
      -- AND: Prepare expectation
      let expError =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "branch"
                  [("app_uuid", U.toString defaultApp.uuid), ("uuid", nonExistingBranchUuid)]
              )
      -- WHEN:
      (c1, s1) <- createConnection appContext (reqUrlT (u' nonExistingBranchUuid) (Just reqAuthToken))
      -- THEN:
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]
  it "WS 404 NOT FOUND - questionnaire was deleted" $
    -- GIVEN: Prepare database
    do
      let branch = amsterdamBranch
      let branchData = amsterdamBranchData
      insertBranchAndUsers appContext branch branchData
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext branch.uuid
      -- AND: Prepare expectation
      let expError = NotExistsError (_ERROR_SERVICE_QTN_COLLABORATION__FORCE_DISCONNECT (U.toString $ branch.uuid))
      -- WHEN: Update permission
      runInContext (deleteBranch branch.uuid) appContext
      -- THEN: Read response
      read_Error c1 expError
      read_Error c2 expError
      -- AND: Close sockets
      closeSockets [s1, s2]
