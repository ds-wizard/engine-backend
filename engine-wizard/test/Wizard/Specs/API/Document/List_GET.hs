module Wizard.Specs.API.Document.List_GET
  ( list_GET
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Document.DocumentMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /documents
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /documents" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/documents"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/documents"
    (Page
       "documents"
       (PageMetadata 20 3 1 0)
       [ toDTO doc1 (Just questionnaire1Simple)
       , toDTO doc2 (Just questionnaire2Simple)
       , toDTO doc3 (Just questionnaire2Simple)
       ])
  create_test_200
    "HTTP 200 OK (query)"
    appContext
    "/documents?q=My exported document 2"
    (Page "documents" (PageMetadata 20 1 1 0) [toDTO doc2 (Just questionnaire2Simple)])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/documents?q=Non-existing document"
    (Page "documents" (PageMetadata 20 0 0 0) [])

create_test_200 title appContext reqUrl expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
     -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (fmap (\x -> x commonWizardTemplate) expDto)
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
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
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
       -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
       -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
