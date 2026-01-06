module Wizard.Specs.Websocket.Project.Detail.GeneralSpec where

import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Project.ProjectMapper
import Wizard.Service.Project.ProjectService

import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Project.Detail.Common

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
      let project = project10
      insertProjectAndUsers appContext project
      -- WHEN
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext project.uuid
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
    project1
    Nothing
    "View Project"
  create_403_no_perm
    "WS 403 FORBIDDEN - no required view entity permission (Non-owner, Private)"
    appContext
    project1
    (Just reqNonAdminAuthToken)
    "View Project"
  it "WS 403 FORBIDDEN - when perms are changed" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers appContext project
      let updatedProject = project {visibility = PrivateProjectVisibility, sharing = RestrictedProjectSharing}
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext project.uuid
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"
      -- WHEN: Update permission
      runInContext (modifyProjectShare updatedProject.uuid (toChangeDTO updatedProject)) appContext
      -- THEN: Read response
      read_SetUserList c1 1
      read_SetUserList c1 0
      read_SetUserList_or_Error c2 expError
      read_SetUserList_or_Error c3 expError
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

create_403_no_perm title appContext project authToken errorMessage =
  it title $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers appContext project
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN errorMessage
      -- WHEN: Connect to websocket
      (c1, s1) <- createConnection appContext (reqUrlT project.uuid authToken)
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
      let nonExistingProjectUuid = "fd5ea37c-852a-4174-9d65-2bf23202541d"
      -- AND: Prepare expectation
      let expError =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "project"
                  [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", nonExistingProjectUuid)]
              )
      -- WHEN:
      (c1, s1) <- createConnection appContext (reqUrlT (u' nonExistingProjectUuid) (Just reqAuthToken))
      -- THEN:
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]
  it "WS 404 NOT FOUND - project was deleted" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers appContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext project.uuid
      -- AND: Prepare expectation
      let expError = NotExistsError (_ERROR_SERVICE_PROJECT_COLLABORATION__FORCE_DISCONNECT (U.toString $ project.uuid))
      -- WHEN: Update permission
      runInContext (deleteProject project.uuid True) appContext
      -- THEN: Read response
      read_Error c1 expError
      read_Error c2 expError
      read_Error c3 expError
      -- AND: Close sockets
      closeSockets [s1, s2, s3]
