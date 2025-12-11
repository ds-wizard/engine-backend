module Wizard.Specs.API.Project.Event.Detail_GET (
  detail_GET,
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
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectPermDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Event.ProjectEventLenses ()
import Wizard.Model.Project.Project
import Wizard.Service.Project.Event.ProjectEventMapper
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/events/{eventUuid}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/projects/{projectUuid}/events/{eventUuid}" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid projectEventUuid =
  BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/events/" ++ U.toString projectEventUuid

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext project1 project1Events (sre_rQ1' project1Uuid) [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView)" appContext project2 project2Events (sre_rQ1' project2Uuid) [reqNonAdminAuthHeader]
  create_test_200
    "HTTP 200 OK (Commenter)"
    appContext
    (project13 {visibility = PrivateProjectVisibility})
    project13Events
    (sre_rQ1' project13Uuid)
    [reqNonAdminAuthHeader]
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    appContext
    project13
    project13Events
    (sre_rQ1' project13Uuid)
    [reqIsaacAuthTokenHeader]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (project13 {sharing = AnyoneWithLinkCommentProjectSharing})
    project13Events
    (sre_rQ1' project13Uuid)
    []
  create_test_200 "HTTP 200 OK (Anonymous, VisibleView, Sharing)" appContext project7 project7Events (sre_rQ1' project7Uuid) []
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" appContext project3 project3Events (sre_rQ1' project3Uuid) [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing)" appContext project10 project10Events (sre_rQ1' project10Uuid) []

create_test_200 title appContext project projectEvents projectEvent authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid (getUuid projectEvent)
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toEventDTO projectEvent (Just userAlbert)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO deleteProjectVersions appContext
      runInContextIO deleteProjectEvents appContext
      runInContextIO deleteProjectComments appContext
      runInContextIO deleteProjectCommentThreads appContext
      runInContextIO deleteProjectPerms appContext
      runInContextIO deleteProjects appContext
      runInContextIO (insertProject project) appContext
      runInContextIO (insertProjectEvents projectEvents) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    project1
    (sre_rQ1' project1Uuid)
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    project2
    (sre_rQ1' project2Uuid)
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    project3
    (sre_rQ1' project3Uuid)
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext project projectEvent authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid (getUuid projectEvent)
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/events"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
