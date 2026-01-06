module Wizard.Specs.API.Project.User.List_Suggestions_GET (
  list_suggestions_GET,
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
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import Wizard.Service.User.UserMapper
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/users/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/projects/{projectUuid}/users/suggestions" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid query = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/users/suggestions?sort=uuid,asc" ++ query

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
    [reqAuthHeader]
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestion . toSimple) [userAlbert]))
  create_test_200
    "HTTP 200 OK (Commenter)"
    appContext
    (project13 {visibility = PrivateProjectVisibility})
    [reqNonAdminAuthHeader]
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestion . toSimple) [userNikola]))
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    appContext
    project13
    [reqIsaacAuthTokenHeader]
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (project13 {sharing = AnyoneWithLinkCommentProjectSharing})
    []
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    project3
    [reqNonAdminAuthHeader]
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    appContext
    project10
    []
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))

create_test_200 title appContext project authHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO (insertProject project) appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
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
    (_ERROR_VALIDATION__FORBIDDEN "Comment Project")
  create_test_403
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    project2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Comment Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    appContext
    project7
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
      let reqUrl = reqUrlT project.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO (insertProject project) appContext
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/users/suggestions"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
