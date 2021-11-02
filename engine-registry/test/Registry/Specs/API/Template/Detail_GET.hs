module Registry.Specs.API.Template.Detail_GET
  ( detail_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.Template.TemplateDetailJM ()
import Registry.Database.Migration.Development.Template.Data.Templates
import qualified Registry.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Registry.Model.Context.AppContext

import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /templates/{tmlId}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith ((), Application)
detail_get appContext =
  describe "GET /templates/{tmlId}" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/templates/global:questionnaire-report:1.0.0"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = commonWizardTemplateDetailDTO
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
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
    "/templates/global:non-existing-template:1.0.0"
    reqHeaders
    reqBody
    "template"
    [("id", "global:non-existing-template:1.0.0")]
