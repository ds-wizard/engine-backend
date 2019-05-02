module Specs.API.Questionnaire.List_GET
  ( list_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Questionnaire.QuestionnaireDTO
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import qualified Database.Migration.Development.User.UserMigration
       as U
import LensesConfig
import Model.Context.AppContext
import Service.Questionnaire.QuestionnaireMapper

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
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
    let expDto = [toSimpleDTO questionnaire1 germanyPackage]
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK (User which isn't admin and owner)" $
     -- GIVEN: Prepare request
   do
    let reqHeaders = [reqNonAdminAuthHeader]
    -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = [] :: [QuestionnaireDTO]
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "QTN_PERM"
