module Registry.Specs.API.Template.List_GET
  ( list_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Registry.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Registry.Model.Context.AppContext
import Shared.Database.Migration.Development.Template.Data.Templates

import Registry.Specs.API.Common
import Registry.Specs.Common

-- ------------------------------------------------------------------------
-- GET /templates
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith ((), Application)
list_get appContext = describe "GET /templates" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/templates"

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
    let expDto = [commonWizardTemplate]
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
