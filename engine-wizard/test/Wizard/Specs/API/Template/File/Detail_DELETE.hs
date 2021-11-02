module Wizard.Specs.API.Template.File.Detail_DELETE
  ( detail_delete
  ) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.File.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /templates/{tmlId}/files/{fileUuid}
-- ------------------------------------------------------------------------
detail_delete :: AppContext -> SpecWith ((), Application)
detail_delete appContext =
  describe "DELETE /templates/{tmlId}/files/{fileUuid}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/templates/global:questionnaire-report:1.0.0/files/7f83f7ce-4096-49a5-88d1-bd509bf72a9b"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  create_test_204 "HTTP 204 NO CONTENT (user token)" appContext reqAuthHeader
  create_test_204 "HTTP 204 NO CONTENT (service token)" appContext reqServiceHeader

create_test_204 title appContext reqAuthHeader =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertAbsenceOfTemplateFileInDB appContext templateFileDefaultHtml

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "TML_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/templates/global:questionnaire-report:1.0.0/files/7f83f7ce-4096-49a5-88d1-bd509bf72a9b"
    (reqHeadersT reqAuthHeader)
    reqBody
    "template_file"
    [("uuid", "7f83f7ce-4096-49a5-88d1-bd509bf72a9b")]
