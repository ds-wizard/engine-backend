module Wizard.Specs.API.Questionnaire.List_GET
  ( list_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith ((), Application)
list_get appContext =
  describe "GET /questionnaires" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaires"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Admin)"
    appContext
    "/questionnaires"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire2Dto, questionnaire3Dto])
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    appContext
    "/questionnaires?page=1&size=1"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 1 3 3 1) [questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    appContext
    "/questionnaires?q=pr"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire1Dto])
  create_test_200
    "HTTP 200 OK (isTemplate - true)"
    appContext
    "/questionnaires?isTemplate=true"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire1Dto])
  create_test_200
    "HTTP 200 OK (isTemplate - false)"
    appContext
    "/questionnaires?isTemplate=false"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire2Dto, questionnaire3Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    appContext
    "/questionnaires?sort=name,asc"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire3Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    appContext
    "/questionnaires?sort=updatedAt,desc"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire3Dto, questionnaire1Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin)"
    appContext
    "/questionnaires?sort=uuid,asc"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire3Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query)"
    appContext
    "/questionnaires?q=pr"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 0 0 0) [])
  create_test_200
    "HTTP 200 OK (isTemplate - true)"
    appContext
    "/questionnaires?isTemplate=true"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 0 0 0) [])
  create_test_200
    "HTTP 200 OK (isTemplate - false)"
    appContext
    "/questionnaires?isTemplate=false"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire3Dto, questionnaire2Dto])

create_test_200 title appContext reqUrl reqAuthHeader expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
       -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO TML.runMigration appContext
    runInContextIO QTN.runMigration appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "QTN_PERM"
