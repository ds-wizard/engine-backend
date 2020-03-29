module Wizard.Specs.API.User.Detail_DELETE
  ( detail_delete
  ) where

import Control.Lens ((^.))
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.API.Document.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /users/{uUuid}
-- ------------------------------------------------------------------------
detail_delete :: AppContext -> SpecWith Application
detail_delete appContext =
  describe "DELETE /users/{uUuid}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
    runInContextIO DOC.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Compare state in DB with expectation
    assertAbsenceOfUserInDB appContext userAlbert
    assertAbsenceOfQuestionnaireInDB appContext questionnaire1
    assertAbsenceOfQuestionnaireInDB appContext questionnaire2
    assertAbsenceOfDocumentInDB appContext doc3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. serverConfig) reqMethod reqUrl [] "" "UM_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "user"
    "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
