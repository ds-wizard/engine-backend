module Wizard.Specs.API.Document.List_Housekeeping_GET
  ( list_housekeeping_GET
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /documents/housekeeping
-- ------------------------------------------------------------------------
list_housekeeping_GET :: AppContext -> SpecWith Application
list_housekeeping_GET appContext =
  describe "GET /documents/housekeeping" $ do
    test_204 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/documents/housekeeping"

reqHeaders = [reqServiceHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
    runInContextIO (insertDocument doc4) appContext
    runInContextIO (insertDocument doc5) appContext
    runInContextIO (insertDocument doc6) appContext
    runInContextIO (insertDocument tempDocQueued) appContext
    runInContextIO (insertDocument tempDocInProgress) appContext
    runInContextIO (insertDocument tempDocDone) appContext
    runInContextIO (insertDocument tempDocError) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 8

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext =
  it "HTTP 401 UNAUTHORIZED" $
     -- GIVEN: Prepare request
   do
    let reqHeaders = []
     -- GIVEN: Prepare expectation
    let expStatus = 401
    let expHeaders = resCorsHeaders
    let expDto = createUnauthorizedError _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
