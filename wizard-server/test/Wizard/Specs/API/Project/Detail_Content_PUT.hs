module Wizard.Specs.API.Project.Detail_Content_PUT (
  detail_content_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Project.ProjectContentChangeDTO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/projects/{projectUuid}/content
-- ------------------------------------------------------------------------
detail_content_PUT :: AppContext -> SpecWith ((), Application)
detail_content_PUT appContext =
  describe "PUT /wizard-api/projects/{projectUuid}/content" $ do
    test_200 appContext
    test_400 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/content"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDto projectUuid =
  ProjectContentChangeDTO
    { events = [toEventChangeDTO (slble_rQ2' projectUuid)]
    }

reqBody projectUuid = encode (reqDto projectUuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext project1 project1EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Owner, VisibleView)" appContext project2 project2EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, Public)" appContext project3 project3EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing" appContext project10 project10EventsEdited []

create_test_200 title appContext project projectEventsEdited authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = reqDto project.uuid
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (insertProject project7) appContext
      runInContextIO (insertProjectEvents project7Events) appContext
      runInContextIO (traverse_ insertProjectVersion project7Versions) appContext
      runInContextIO (insertProject project10) appContext
      runInContextIO (insertProjectEvents project10Events) appContext
      runInContextIO (traverse_ insertProjectVersion project10Versions) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBody project.uuid)
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectContentChangeDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectDtos resBody expDto
      -- AND: Find a result in DB
      assertExistenceOfProjectContentInDB appContext project.uuid projectEventsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT project3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    project1
    project1EventsEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    project2
    project2EventsEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    appContext
    project7
    project7EventsEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    project3
    project3EventsEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext project projectEventsEdited authHeader reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (insertProject project7) appContext
      runInContextIO (insertProject project10) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBody project.uuid)
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/content"
    (reqHeadersT [reqAuthHeader])
    (reqBody $ u' "f08ead5f-746d-411b-aee6-77ea3d24016a")
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
