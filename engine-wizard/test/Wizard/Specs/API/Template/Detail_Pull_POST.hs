module Wizard.Specs.API.Template.Detail_Pull_POST
  ( detail_pull_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.DAO.Template.TemplateDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /templates/{tmlId}
-- ------------------------------------------------------------------------
detail_pull_post :: AppContext -> SpecWith ((), Application)
detail_pull_post appContext =
  describe "POST /templates/{tmlId}/pull" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/templates/" ++ (commonWizardTemplate ^. tId) ++ "/pull"

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
    runInContextIO deleteTemplates appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findTemplates appContext 1
    assertExistenceOfTemplateInDB appContext commonWizardTemplate

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "TML_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  it "HTTP 404 NOT FOUND - Template was not found in Registry" $
      -- GIVEN: Prepare request
   do
    let reqUrl = "/templates/global:non-existing-template:1.0.0/pull"
    let reqHeaders = reqHeadersT reqAuthHeader
     -- AND: Prepare expectation
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createNotExistsError (_ERROR_SERVICE_TB__PULL_NON_EXISTING_TML "global:non-existing-template:1.0.0")
    let expBody = encode expDto
      -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
