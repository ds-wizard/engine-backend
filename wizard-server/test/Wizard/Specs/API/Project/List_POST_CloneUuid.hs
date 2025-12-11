module Wizard.Specs.API.Project.List_POST_CloneUuid (
  list_POST_cloneUuid,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.ProjectCreateJM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects?cloneUuid={projectUuid}
-- ------------------------------------------------------------------------
list_POST_cloneUuid :: AppContext -> SpecWith ((), Application)
list_POST_cloneUuid appContext =
  describe "POST /wizard-api/projects/{projectUuid}/clone" $ do
    test_201 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/clone"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 200 OK (Owner, Private)" appContext project1Dto
  create_test_201 "HTTP 200 OK (Owner, VisibleView)" appContext project2Dto
  create_test_201 "HTTP 200 OK (Non-Owner, VisibleEdit)" appContext project3Dto

create_test_201 title appContext project =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ project.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = project
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectCloneDtos resBody expDto
      -- AND: Find a result in DB
      assertCountInDB findProjects appContext 4

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT project3.uuid) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext project1 "View Project"

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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/clone"
    (reqHeadersT reqAuthHeader)
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
