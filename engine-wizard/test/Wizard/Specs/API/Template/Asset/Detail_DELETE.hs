module Wizard.Specs.API.Template.Asset.Detail_DELETE
  ( detail_delete
  ) where

import Control.Lens ((^.))
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.Asset.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /templates/{tmlId}/assets/{fileUuid}
-- ------------------------------------------------------------------------
detail_delete :: AppContext -> SpecWith ((), Application)
detail_delete appContext =
  describe "DELETE /templates/{tmlId}/assets/{fileUuid}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/templates/dsw:default-template:1.0.0/assets/6c367648-9b60-4307-93b2-0851938adee0"

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
    assertAbsenceOfTemplateAssetInDB appContext templateAssetLogo (commonWizardTemplate ^. tId)

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
  createNotFoundTest
    reqMethod
    "/templates/dsw:default-template:1.0.0/assets/6c367648-9b60-4307-93b2-0851938adee0"
    (reqHeadersT reqAuthHeader)
    reqBody
    "template"
    "dsw:default-template:1.0.0"
