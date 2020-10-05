module Wizard.Specs.API.Questionnaire.Detail_Report_Preview_POST
  ( detail_report_preview_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.Metric.MetricMigration as MTR
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.Report.Data.Reports
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Report.Report
import Wizard.Service.Questionnaire.QuestionnaireMapper

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /questionnaires/{qtnUuid}/report/preview
-- ------------------------------------------------------------------------
detail_report_preview_post :: AppContext -> SpecWith ((), Application)
detail_report_preview_post appContext =
  describe "POST /questionnaires/{qtnUuid}/report/preview" $ do
    test_200 appContext
    test_400 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/report/preview"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDtoT qtn =
  QuestionnaireContentChangeDTO
    { _questionnaireContentChangeDTOLevel = qtn ^. level
    , _questionnaireContentChangeDTOReplies = M.map toReplyValueDTO (qtn ^. replies)
    , _questionnaireContentChangeDTOLabels = qtn ^. labels
    }

reqBody = encode . reqDtoT $ questionnaire1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView)" appContext questionnaire2 [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView, Sharing)" appContext questionnaire7 []
  create_test_200 "HTTP 200 OK (Non-Owner, Public)" appContext questionnaire3 [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, Public, Sharing)" appContext questionnaire10 []

create_test_200 title appContext qtn authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT authHeader
    let reqDto = reqDtoT qtn
    let reqBody = encode reqDto
     -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = report1
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO (insertPackage germanyPackage) appContext
    runInContextIO (insertQuestionnaire (qtn & replies .~ M.empty)) appContext
    runInContextIO MTR.runMigration appContext
    runInContextIO TML.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, Report)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareReportDtos resBody expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT $ questionnaire3 ^. uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Get Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    questionnaire3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn authHeader errorMessage =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT (qtn ^. uuid)
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createForbiddenError errorMessage
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
    (reqHeadersT [reqAuthHeader])
    reqBody
    "questionnaire"
    "f08ead5f-746d-411b-aee6-77ea3d24016a"
