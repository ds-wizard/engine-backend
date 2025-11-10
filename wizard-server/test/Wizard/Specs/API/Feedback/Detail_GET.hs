module Wizard.Specs.API.Feedback.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as F
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Model.Context.AppContext
import Wizard.Service.Feedback.FeedbackMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Feedback.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/feedbacks/{feedbackUuid}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/feedbacks/{feedbackUuid}" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/feedbacks/c44c06d1-ad9f-4f73-9c05-2aa9eddacae1"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toDTO appContext.serverConfig defaultQuestionnaire feedback1
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO loadFeedbackTokenFromEnv appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO F.runMigration appContext
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
  createNotFoundTest'
    reqMethod
    "/wizard-api/feedbacks/f335e2b9-5e81-4412-8bf5-50753b2020fc"
    reqHeaders
    reqBody
    "feedback"
    [("uuid", "f335e2b9-5e81-4412-8bf5-50753b2020fc")]
