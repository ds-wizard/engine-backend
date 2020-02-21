module Wizard.Specs.API.Feedback.List_GET
  ( list_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as F
import Wizard.Model.Context.AppContext
import Wizard.Service.Feedback.FeedbackMapper
import Wizard.Service.Feedback.FeedbackService

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /feedbacks
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext = describe "GET /feedbacks" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/feedbacks"

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
    let appConfig = appContext ^. applicationConfig
    let iUrl1 = createIssueUrl appConfig feedback1
    let iUrl2 = createIssueUrl appConfig feedback2
    let expDto = [toDTO feedback1 iUrl1, toDTO feedback2 iUrl2]
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO F.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
