module Wizard.Specs.API.Template.List_GET
  ( list_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Model.Template.TemplateJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /templates
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith ((), Application)
list_get appContext =
  describe "GET /templates" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/templates"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (user token)" appContext reqAuthHeader
  create_test_200 "HTTP 200 OK (service token)" appContext reqServiceHeader

create_test_200 title appContext reqAuthHeader =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = [commonWizardTemplateSimpleDTO]
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "DMP_PERM"
