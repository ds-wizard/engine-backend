module Wizard.Specs.API.Questionnaire.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaires
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/questionnaires" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/questionnaires"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&page=1&size=1"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 1 6 6 1) [questionnaire14Dto])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&q=pr"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire1Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids)"
    appContext
    (BS.pack $ "/wizard-api/questionnaires?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire2Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, or)"
    appContext
    ( BS.pack $
        "/wizard-api/questionnaires?sort=uuid,asc&userUuidsOp=or&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire1Dto, questionnaire2Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, and)"
    appContext
    ( BS.pack $
        "/wizard-api/questionnaires?sort=uuid,asc&userUuidsOp=and&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 0 0 0) ([] :: [QuestionnaireDTO]))
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - true)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isTemplate=true"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire14Dto, questionnaire1Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - false)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isTemplate=false"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire3Dto, questionnaire15Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - isMigrating - true)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isMigrating=true"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 0 0 0) ([] :: [QuestionnaireDTO]))
  create_test_200
    "HTTP 200 OK (Admin - isMigrating - false)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isMigrating=false"
    reqAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 6 1 0)
        [questionnaire3Dto, questionnaire14Dto, questionnaire1Dto, questionnaire15Dto, questionnaire2Dto, questionnaire12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&projectTags=projectTag1"
    reqAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 4 1 0)
        [questionnaire14Dto, questionnaire1Dto, questionnaire2Dto, questionnaire12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, or)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&projectTagsOp=or&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 4 1 0)
        [questionnaire14Dto, questionnaire1Dto, questionnaire2Dto, questionnaire12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, and)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&projectTagsOp=and&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Admin - knowledgePackage)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam:core-amsterdam:all"
    reqAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire14Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc"
    reqAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 6 1 0)
        [questionnaire3Dto, questionnaire14Dto, questionnaire1Dto, questionnaire15Dto, questionnaire2Dto, questionnaire12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    appContext
    "/wizard-api/questionnaires?sort=updatedAt,desc"
    reqAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 6 1 0)
        [questionnaire15Dto, questionnaire3Dto, questionnaire14Dto, questionnaire1Dto, questionnaire12Dto, questionnaire2Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc"
    reqNonAdminAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 5 1 0)
        [questionnaire3Dto, questionnaire14Dto, questionnaire15Dto, questionnaire2Dto, questionnaire12Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin - query)"
    appContext
    "/wizard-api/questionnaires?q=pr"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query users)"
    appContext
    (BS.pack $ "/wizard-api/questionnaires?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire2Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - projectTags)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&projectTags=projectTag1"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire14Dto, questionnaire2Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - knowledgeModelPackage)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam:core-amsterdam:all"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 1 1 0) [questionnaire14Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - true)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isTemplate=true"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 2 1 0) [questionnaire14Dto, questionnaire12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - false)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isTemplate=false"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 3 1 0) [questionnaire3Dto, questionnaire15Dto, questionnaire2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isMigrating - true)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isMigrating=true"
    reqNonAdminAuthHeader
    (Page "questionnaires" (PageMetadata 20 0 0 0) ([] :: [QuestionnaireDTO]))
  create_test_200
    "HTTP 200 OK (Non-Admin - isMigrating - false)"
    appContext
    "/wizard-api/questionnaires?sort=uuid,asc&isMigrating=false"
    reqNonAdminAuthHeader
    ( Page
        "questionnaires"
        (PageMetadata 20 5 1 0)
        [questionnaire3Dto, questionnaire14Dto, questionnaire15Dto, questionnaire2Dto, questionnaire12Dto]
    )

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
      runInContextIO (insertPackage amsterdamKmPackage) appContext
      runInContextIO (insertQuestionnaire questionnaire12) appContext
      runInContextIO (insertQuestionnaire questionnaire14) appContext
      runInContextIO (insertQuestionnaire questionnaire15) appContext
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
