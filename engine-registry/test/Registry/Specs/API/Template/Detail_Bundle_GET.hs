module Registry.Specs.API.Template.Detail_Bundle_GET
  ( detail_bundle_get
  ) where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import qualified Registry.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Registry.Model.Context.AppContext
import Shared.Database.Migration.Development.Template.Data.Templates

import Registry.Specs.API.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /templates/{tmlId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_get :: AppContext -> SpecWith ((), Application)
detail_bundle_get appContext =
  describe "GET /templates/{tmlId}/bundle" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/templates/" ++ (commonWizardTemplate ^. tId) ++ "/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCorsHeadersPlain
     -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/templates/global:non-existing-template:1.0.0/bundle"
    reqHeaders
    reqBody
    "template"
    [("id", "global:non-existing-template:1.0.0")]
