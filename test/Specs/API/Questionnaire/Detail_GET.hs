module Specs.API.Questionnaire.Detail_GET
  ( detail_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import qualified Database.Migration.Development.User.UserMigration
       as U
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.Questionnaire.QuestionnaireMapper

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith Application
detail_get appContext =
  describe "GET /questionnaires/{qtnUuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaires/af984a75-56e3-49f8-b16f-d6b99599910a"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK (Admin)" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = toDetailWithPackageWithEventsDTO questionnaire1 germanyPackage km1WithQ4
    let expBody = encode expDto
     -- AND: Run migrations
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "QTN_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = do
  createNotFoundTest reqMethod "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a" reqHeaders reqBody
  it "HTTP 404 OK (User which isn't admin and owner)" $
     -- GIVEN: Prepare request
   do
    let reqHeaders = [reqNonAdminAuthHeader]
    -- AND: Prepare expectation
    let expStatus = 404
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = NotExistsError _ERROR_DATABASE__ENTITY_NOT_FOUND
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
