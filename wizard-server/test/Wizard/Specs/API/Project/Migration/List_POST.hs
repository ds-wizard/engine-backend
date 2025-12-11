module Wizard.Specs.API.Project.Migration.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationJM ()
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectMigrations
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.Project.ProjectMigrationMigration as PRJ_MIG
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Migration.ProjectMigration
import Wizard.Model.Project.Project

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Migration.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects/{projectUuid}/migrations
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/projects/{projectUuid}/migrations" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/migrations"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDto = projectMigrationCreateDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201
    "HTTP 201 CREATED (Owner, Private)"
    appContext
    project4
    project4Events
    project4Upgraded
    project4UpgradedEvents
    projectMigration
    projectMigrationDto
    reqNonAdminAuthHeader
  create_test_201
    "HTTP 201 CREATED (Non-Owner, VisibleView)"
    appContext
    project4VisibleView
    project4VisibleViewEvents
    project4VisibleViewUpgraded
    project4VisibleViewUpgradedEvents
    projectMigration
    projectMigrationVisibleViewDto
    reqNonAdminAuthHeader
  create_test_201
    "HTTP 201 CREATED (Non-Owner, Public)"
    appContext
    project4VisibleEdit
    project4VisibleEditEvents
    project4VisibleEditUpgraded
    project4VisibleEditUpgradedEvents
    projectMigration
    projectMigrationVisibleEditDto
    reqNonAdminAuthHeader

create_test_201 title appContext oldProject oldProjectEvents newProject newProjectEvents state stateDto authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ oldProject.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      let expDto = stateDto {resolvedQuestionUuids = []} :: ProjectMigrationDTO
      let expBody = encode expDto
      -- AND: Prepare database
      runInContextIO TML.runMigration appContext
      runInContextIO (insertProject oldProject) appContext
      runInContextIO (insertProjectEvents oldProjectEvents) appContext
      runInContextIO (insertProject newProject) appContext
      runInContextIO (insertProjectEvents newProjectEvents) appContext
      runInContextIO (insertProject differentProject) appContext
      runInContextIO (insertProjectEvents differentProjectEvents) appContext
      runInContextIO PRJ_MIG.runMigration appContext
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectMigrationDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectMigratorDtos resBody expDto
      -- AND: Find a result in DB
      let entityInDB =
            state
              { newProjectUuid = resBody.newProject.uuid
              , resolvedQuestionUuids = []
              }
      assertExistenceOfMigrationStateInDB appContext entityInDB

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT project4.uuid) "targetKnowledgeModelPackageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT project4.uuid) [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest appContext reqMethod (reqUrlT project3.uuid) [reqCtHeader] reqBody "PRJ_PERM"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext project1 "Migrate Project"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" appContext project2 "Migrate Project"

create_test_403 title appContext project reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ project.uuid
      let reqHeaders = reqHeadersT reqNonAdminAuthHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findProjectMigrations appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    (reqUrlT project4.uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "project"
    [("uuid", "57250a07-a663-4ff3-ac1f-16530f2c1bfe")]
