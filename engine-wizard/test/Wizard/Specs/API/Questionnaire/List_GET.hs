module Wizard.Specs.API.Questionnaire.List_GET
  ( list_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import Wizard.Database.Migration.Development.User.Data.Users
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
    "HTTP 200 OK (Admin - pagination)"
    appContext
    "/questionnaires?page=1&size=1"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 1 4 4 1) [questionnaire1Dto])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    appContext
    "/questionnaires?sort=name,asc&q=pr"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire1Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids)"
    appContext
    (BS.pack $ "/questionnaires?sort=name,asc&userUuids=" ++ U.toString (userAlbert ^. uuid))
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, or)"
    appContext
    (BS.pack $
     "/questionnaires?sort=name,asc&userUuidsOp=or&userUuids=" ++
     U.toString (userAlbert ^. uuid) ++ "," ++ U.toString (userIsaac ^. uuid))
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, and)"
    appContext
    (BS.pack $
     "/questionnaires?sort=name,asc&userUuidsOp=and&userUuids=" ++
     U.toString (userAlbert ^. uuid) ++ "," ++ U.toString (userIsaac ^. uuid))
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 0 0 0) [])
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - true)"
    appContext
    "/questionnaires?sort=name,asc&isTemplate=true"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire1Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - false)"
    appContext
    "/questionnaires?sort=name,asc&isTemplate=false"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire3Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - projectTags)"
    appContext
    "/questionnaires?sort=name,asc&projectTags=projectTag1"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - projectTags, or)"
    appContext
    "/questionnaires?sort=name,asc&projectTagsOp=or&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - projectTags, and)"
    appContext
    "/questionnaires?sort=name,asc&projectTagsOp=and&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    appContext
    "/questionnaires?sort=name,asc"
    reqAuthHeader
    (Page
       "questionnaires"
       (PageMetadata 20 4 1 0)
       [questionnaire1Dto, questionnaire12Dto, questionnaire3Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    appContext
    "/questionnaires?sort=updatedAt,desc"
    reqAuthHeader
    (Page
       "questionnaires"
       (PageMetadata 20 4 1 0)
       [questionnaire3Dto, questionnaire1Dto, questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin)"
    appContext
    "/questionnaires?sort=uuid,asc"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire3Dto, questionnaire2Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query)"
    appContext
    "/questionnaires?q=pr"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query users)"
    appContext
    (BS.pack $ "/questionnaires?sort=name,asc&userUuids=" ++ U.toString (userAlbert ^. uuid))
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - projectTags)"
    appContext
    "/questionnaires?sort=name,asc&projectTags=projectTag1"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire12Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - true)"
    appContext
    "/questionnaires?isTemplate=true"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - false)"
    appContext
    "/questionnaires?isTemplate=false&sort=name,asc"
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
    runInContextIO (insertQuestionnaire questionnaire12) appContext
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
