module Wizard.Specs.API.Document.List_GET
  ( list_GET
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import Wizard.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Document.DocumentMapper

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /documents
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /documents" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/documents"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 CREATED (Admin)"
    appContext
    (Page
       "documents"
       (PageMetadata 0 3 1 0)
       [toDTO doc1 (Just questionnaire1Dto), toDTO doc2 (Just questionnaire2Dto), toDTO doc3 (Just questionnaire2Dto)])
    reqAuthHeader
  create_test_200
    "HTTP 200 CREATED (Non-Admin)"
    appContext
    (Page "documents" (PageMetadata 0 2 1 0) [toDTO doc1 Nothing, toDTO doc2 (Just questionnaire2Dto)])
    reqNonAdminAuthHeader

create_test_200 title appContext expDto authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    (Right commonWizardTemplate) <- runInContextIO getCommonWizardTemplate appContext
    let expBody = encode (fmap (\x -> x commonWizardTemplate) expDto)
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "DMP_PERM"
