module Specs.API.Questionnaire.Detail_Report_GET
  ( detail_report_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Report.ReportDTO
import Api.Resource.Report.ReportJM ()
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.Metric.Data.Metrics
import qualified
       Database.Migration.Development.Metric.MetricMigration as MTR
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import LensesConfig
import Model.Context.AppContext
import Service.Questionnaire.QuestionnaireMapper
import Util.List (elems)

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/{qtnUuid}/report
-- ------------------------------------------------------------------------
detail_report_get :: AppContext -> SpecWith Application
detail_report_get appContext =
  describe "GET /questionnaires/{qtnUuid}/report" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaires/af984a75-56e3-49f8-b16f-d6b99599910a/report"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toDetailWithPackageWithEventsDTO questionnaire1Edited netherlandsPackageV2 km1NetherlandsV2
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
    runInContextIO MTR.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) headers body) = response
    liftIO $ status `shouldBe` expStatus
    liftIO $ (expHeaders `elems` headers) `shouldBe` True
    -- AND: Compare body
    let (Right resBody) = eitherDecode body :: Either String ReportDTO
    let rs = resBody ^. chapterReports
    liftIO $ (length rs) `shouldBe` 3
    -- Chapter report 1
    let r1 = rs !! 0
    liftIO $ (r1 ^. chapterUuid) `shouldBe` (chapter1 ^. uuid)
    let (AnsweredIndicationDTO' i1) = (r1 ^. indications) !! 0
    liftIO $ (i1 ^. answeredQuestions) `shouldBe` 3
    liftIO $ (i1 ^. unansweredQuestions) `shouldBe` 0
    liftIO $ (length $ r1 ^. metrics) `shouldBe` 2
    let m11 = (r1 ^. metrics) !! 0
    liftIO $ (m11 ^. metricUuid) `shouldBe` metricI ^. uuid
    liftIO $ (m11 ^. measure) `shouldBe` 1
    let m12 = (r1 ^. metrics) !! 1
    liftIO $ (m12 ^. metricUuid) `shouldBe` metricR ^. uuid
    liftIO $ (m12 ^. measure) `shouldBe` 1
    -- Chapter report 2
    let r2 = rs !! 1
    liftIO $ (r2 ^. chapterUuid) `shouldBe` (chapter2 ^. uuid)
    let (AnsweredIndicationDTO' i2) = (r2 ^. indications) !! 0
    liftIO $ (i2 ^. answeredQuestions) `shouldBe` 10
    liftIO $ (i2 ^. unansweredQuestions) `shouldBe` 1
    liftIO $ (length $ r2 ^. metrics) `shouldBe` 2
    let m21 = (r2 ^. metrics) !! 0
    liftIO $ (m21 ^. metricUuid) `shouldBe` metricF ^. uuid
    liftIO $ (m21 ^. measure) `shouldBe` 1
    let m22 = (r2 ^. metrics) !! 1
    liftIO $ (m22 ^. metricUuid) `shouldBe` metricA ^. uuid
    liftIO $ (m22 ^. measure) `shouldBe` 1
    -- Chapter report 3
    let r3 = rs !! 2
    liftIO $ (r3 ^. chapterUuid) `shouldBe` (chapter3 ^. uuid)
    let (AnsweredIndicationDTO' i3) = (r3 ^. indications) !! 0
    liftIO $ (i3 ^. answeredQuestions) `shouldBe` 2
    liftIO $ (i3 ^. unansweredQuestions) `shouldBe` 0
    liftIO $ (length $ r3 ^. metrics) `shouldBe` 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "QTN_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/report"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire"
    "f08ead5f-746d-411b-aee6-77ea3d24016a"
