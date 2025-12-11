module Wizard.Specs.API.ProjectImporter.List_GET (
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
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import qualified Wizard.Database.Migration.Development.Project.ProjectImporterMigration as ProjectImporterMigration
import Wizard.Model.Context.AppContext
import Wizard.Service.Project.Importer.ProjectImporterMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/project-importers
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/project-importers" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/project-importers"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/project-importers"
    reqAuthHeader
    ( Page
        "projectImporters"
        (PageMetadata 20 3 1 0)
        (fmap toDTO [projectImporterBio3, projectImporterExt1, projectImporterOnto1])
    )
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/wizard-api/project-importers?q=ProjectImporterBio"
    reqAuthHeader
    (Page "projectImporters" (PageMetadata 20 1 1 0) (fmap toDTO [projectImporterBio3]))
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/wizard-api/project-importers?q=Non-existing Project Report"
    reqAuthHeader
    (Page "projectImporters" (PageMetadata 20 0 0 0) ([] :: [ProjectImporterDTO]))

create_test_200 title appContext reqUrl reqAuthHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO ProjectImporterMigration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "PRJ_PERM"
