module Wizard.Specs.API.ProjectImporter.List_Suggestions_GET (
  list_suggestions_GET,
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
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import qualified Wizard.Database.Migration.Development.Project.ProjectImporterMigration as ProjectImporterMigration
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Project.Importer.ProjectImporterMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/project-importers/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/project-importers/suggestions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/project-importers/suggestions"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/project-importers/suggestions?enabled=true"
    reqAuthHeader
    ( Page
        "projectImporters"
        (PageMetadata 20 2 1 0)
        (fmap toDTO [projectImporterBio2, projectImporterExt1])
    )
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/wizard-api/project-importers/suggestions?enabled=true&q=ProjectImporterBio"
    reqAuthHeader
    (Page "projectImporters" (PageMetadata 20 1 1 0) (fmap toDTO [projectImporterBio2]))
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/wizard-api/project-importers/suggestions?enabled=true&q=Non-existing Project Report"
    reqAuthHeader
    (Page "projectImporters" (PageMetadata 20 0 0 0) ([] :: [ProjectImporterDTO]))
  create_test_200
    "HTTP 200 OK (query 'projectUuid')"
    appContext
    "/wizard-api/project-importers/suggestions?enabled=true&projectUuid=af984a75-56e3-49f8-b16f-d6b99599910a"
    reqAuthHeader
    (Page "projectImporters" (PageMetadata 20 1 1 0) (fmap toDTO [projectImporterBio2]))

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
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
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
