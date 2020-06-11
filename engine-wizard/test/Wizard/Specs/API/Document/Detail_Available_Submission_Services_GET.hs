module Wizard.Specs.API.Document.Detail_Available_Submission_Services_GET
  ( detail_available_submission_Services_GET
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
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Submission.SubmissionMapper

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------------------
-- GET /documents/264ca352-1a99-4ffd-860e-32aee9a98428/available-submission-services
-- ------------------------------------------------------------------------------------
detail_available_submission_Services_GET :: AppContext -> SpecWith ((), Application)
detail_available_submission_Services_GET appContext =
  describe "GET /documents/264ca352-1a99-4ffd-860e-32aee9a98428/available-submission-services" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/documents/264ca352-1a99-4ffd-860e-32aee9a98428/available-submission-services"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = [toSubmissionServiceSimpleDTO defaultSubmissionService]
    let expBody = encode expDto
    -- AND: Run migrations
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
test_403 appContext = do
  createNoPermissionTest (appContext ^. serverConfig) reqMethod reqUrl [reqCtHeader] reqBody "SUBM_PERM"
  it "HTTP 403 FORBIDDEN - Doc is not accessible for user" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/documents/35ef63fd-cb5c-448c-9a4f-54b572573c20/available-submission-services"
    let reqHeaders = [reqNonAdminAuthHeader]
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCorsHeaders
    let expDto = createForbiddenError (_ERROR_VALIDATION__FORBIDDEN "Detail Document")
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
