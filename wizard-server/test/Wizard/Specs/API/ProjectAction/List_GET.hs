module Wizard.Specs.API.ProjectAction.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Wizard.Database.Migration.Development.Project.Data.ProjectActions
import qualified Wizard.Database.Migration.Development.Project.ProjectActionMigration as ProjectAction_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Project.Action.ProjectActionMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/project-actions
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/project-actions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/project-actions"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/project-actions"
    reqAuthHeader
    (Page "projectActions" (PageMetadata 20 3 1 0) [projectActionFtp3, projectActionMail1, projectActionScp1])
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/wizard-api/project-actions?q=FTP"
    reqAuthHeader
    (Page "projectActions" (PageMetadata 20 1 1 0) [projectActionFtp3])
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/wizard-api/project-actions?q=Non-existing Project Report"
    reqAuthHeader
    (Page "projectActions" (PageMetadata 20 0 0 0) [])

create_test_200 title appContext reqUrl reqAuthHeader expEntities =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toDTO expEntities
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO ProjectAction_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "PRJ_PERM"
