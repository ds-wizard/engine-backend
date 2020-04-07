module Wizard.Specs.API.Document.Detail_DELETE
  ( detail_DELETE
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /documents/{documentId}
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith Application
detail_DELETE appContext =
  describe "DELETE /documents/{documentId}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/documents/264ca352-1a99-4ffd-860e-32aee9a98428"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  create_test_204 "HTTP 204 NO CONTENT (Admin)" appContext reqAuthHeader
  create_test_204 "HTTP 204 NO CONTENT (Non-Admin)" appContext reqNonAdminAuthHeader

create_test_204 title appContext authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = [reqNonAdminAuthHeader]
     -- AND: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest (appContext ^. serverConfig) reqMethod reqUrl [reqCtHeader] "" "DMP_PERM"
  it "HTTP 403 FORBIDDEN - Qtn is not accessible for user" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/documents/35ef63fd-cb5c-448c-9a4f-54b572573c20"
    let reqHeaders = [reqNonAdminAuthHeader]
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCorsHeaders
    let expDto = createForbiddenError (_ERROR_VALIDATION__FORBIDDEN "Delete Document")
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/documents/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "document"
    "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
