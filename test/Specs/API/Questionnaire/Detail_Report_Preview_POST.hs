module Specs.API.Questionnaire.Detail_Report_Preview_POST
  ( detail_report_preview_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Report.ReportDTO
import Api.Resource.Report.ReportJM ()
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import qualified
       Database.Migration.Development.Metric.MetricMigration as MTR
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import Database.Migration.Development.Report.Data.Reports
import qualified Database.Migration.Development.User.UserMigration
       as U
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.Questionnaire.QuestionnaireMapper
import Service.Report.ReportMapper

import Specs.API.Common
import Specs.API.Questionnaire.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /questionnaires/{qtnUuid}/report/preview
-- ------------------------------------------------------------------------
detail_report_preview_post :: AppContext -> SpecWith Application
detail_report_preview_post appContext =
  describe "POST /questionnaires/{qtnUuid}/report/preview" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/report/preview"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDto =
  QuestionnaireChangeDTO
  { _questionnaireChangeDTOName = questionnaire1Edited ^. name
  , _questionnaireChangeDTOAccessibility = questionnaire1Edited ^. accessibility
  , _questionnaireChangeDTOLevel = 3
  , _questionnaireChangeDTOReplies =
      toReplyDTO <$>
      [ rQ1
      , rQ2
      , rQ2_aYes_fuQ1
      , rQ3
      , rQ4
      , rQ4_it1_itemName
      , rQ4_it1_q5
      , rQ4_it1_q5_it1_itemName
      , rQ4_it1_q5_it1_question7
      , rQ4_it1_q5_it1_question8
      , rQ4_it1_q6
      , rQ4_it2_itemName
      , rQ4_it2_q5
      , rQ4_it2_q6
      , rQ9
      , rQ10
      ]
  }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 reqAuthHeader
  create_test_200 "HTTP 200 OK (Non-Owner, PublicReadOnly)" appContext questionnaire2 reqNonAdminAuthHeader
  create_test_200 "HTTP 200 OK (Non-Owner, Public)" appContext questionnaire3 reqNonAdminAuthHeader

create_test_200 title appContext qtn authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT reqAuthHeader
     -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toReportDTO report1
     -- AND: Run migrations
    runInContextIO (insertPackage germanyPackage) appContext
    runInContextIO (insertQuestionnaire (qtn & replies .~ [])) appContext
    runInContextIO MTR.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ReportDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareReportDtos resBody expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  createInvalidJsonTest
    reqMethod
    (reqUrlT $ questionnaire3 ^. uuid)
    [HJ.json| { name: "Common Questionnaire" } |]
    "level"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT $ questionnaire3 ^. uuid) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest (appContext ^. appConfig) reqMethod (reqUrlT $ questionnaire3 ^. uuid) [] "" "QTN_PERM"
  it "HTTP 403 FORBIDDEN (Non-Owner, Private)" $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT (questionnaire1 ^. uuid)
    let reqHeaders = reqHeadersT reqNonAdminAuthHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Get Questionnaire"
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
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
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/report/preview"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire"
    "f08ead5f-746d-411b-aee6-77ea3d24016a"
