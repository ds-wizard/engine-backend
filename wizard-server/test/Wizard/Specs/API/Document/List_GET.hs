module Wizard.Specs.API.Document.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
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
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Document.DocumentMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/documents
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/documents" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/documents"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/documents"
    ( Page
        "documents"
        (PageMetadata 20 3 1 0)
        [ toDTOWithDocTemplate doc1 project1 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc3 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        ]
    )
  create_test_200
    "HTTP 200 OK (query)"
    appContext
    "/wizard-api/documents?q=My exported document 2"
    (Page "documents" (PageMetadata 20 1 1 0) [toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/wizard-api/documents?q=Non-existing document"
    (Page "documents" (PageMetadata 20 0 0 0) ([] :: [DocumentDTO]))
  create_test_200
    "HTTP 200 OK (documentTemplateId)"
    appContext
    "/wizard-api/documents?documentTemplateId=global:project-report:1.0.0&sort=name,asc"
    ( Page
        "documents"
        (PageMetadata 20 3 1 0)
        [ toDTOWithDocTemplate doc1 project1 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc3 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        ]
    )

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
      runInContextIO DOC_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  it "HTTP 403 FORBIDDEN - only 'admin' can view" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqNonAdminAuthHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError (_ERROR_VALIDATION__FORBIDDEN "Missing permission: DOC_PERM")
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
      runInContextIO DOC_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
