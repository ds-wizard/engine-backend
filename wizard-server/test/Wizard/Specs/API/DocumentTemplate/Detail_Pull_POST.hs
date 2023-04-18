module Wizard.Specs.API.DocumentTemplate.Detail_Pull_POST (
  detail_pull_post,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplate.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /document-templates/{documentTemplateId}
-- ------------------------------------------------------------------------
detail_pull_post :: AppContext -> SpecWith ((), Application)
detail_pull_post appContext =
  describe "POST /document-templates/{documentTemplateId}/pull" $ do
    test_204 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/document-templates/" ++ wizardDocumentTemplate.tId ++ "/pull"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = create_test_204 "HTTP 204 NO CONTENT" appContext reqAuthHeader

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
      runInContextIO deleteDocumentTemplates appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates appContext 1
      assertExistenceOfTemplateInDB appContext wizardDocumentTemplate

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST - DocumentTemplate was not found in Registry" $
    -- GIVEN: Prepare request
    do
      let reqUrl = "/document-templates/global:non-existing-template:1.0.0/pull"
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError (_ERROR_SERVICE_TB__PULL_NON_EXISTING_TML "global:non-existing-template:1.0.0")
      let expBody = encode expDto
      -- WHEN: Call APIA
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "DOC_TML_WRITE_PERM"
