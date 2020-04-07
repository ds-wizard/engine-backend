module Wizard.Specs.API.Submission.List_POST
  ( list_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Api.Resource.Submission.SubmissionJM ()
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /submissions
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /submissions" $ do
    test_201 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/submissions"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = submissionCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 Created" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = submission1
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
test_403 appContext =
  createNoPermissionTest (appContext ^. serverConfig) reqMethod reqUrl [reqCtHeader] reqBody "SUBM_PERM"
