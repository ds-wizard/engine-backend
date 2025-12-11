module Wizard.Specs.API.Project.Version.List_GET (
  list_GET,
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
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/versions
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/projects/{projectUuid}/versions" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/versions"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    project1
    project1Ctn
    [reqAuthHeader]
    [project1AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    project2
    project2Ctn
    [reqNonAdminAuthHeader]
    [project1AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    appContext
    project7
    project7Ctn
    []
    [project1AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    project3
    project3Ctn
    [reqNonAdminAuthHeader]
    []
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing)" appContext project10 project10Ctn [] []

create_test_200 title appContext project projectContent authHeader permissions =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = qVersionsList project.uuid
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (insertProject project7) appContext
      runInContextIO (insertProjectEvents project7Events) appContext
      runInContextIO (traverse_ insertProjectVersion project7Versions) appContext
      runInContextIO (insertProject project10) appContext
      runInContextIO (insertProjectEvents project10Events) appContext
      runInContextIO (traverse_ insertProjectVersion project10Versions) appContext
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
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    project3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/versions"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
