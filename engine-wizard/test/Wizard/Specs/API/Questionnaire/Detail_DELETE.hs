module Wizard.Specs.API.Questionnaire.Detail_DELETE
  ( detail_delete
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
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
detail_delete :: AppContext -> SpecWith ((), Application)
detail_delete appContext =
  describe "DELETE /questionnaires/{qtnUuid}" $ do
    test_204 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  create_test_204 "HTTP 204 NO CONTENT (Owner, Private)" appContext questionnaire1 reqAuthHeader 2
  create_test_204 "HTTP 204 NO CONTENT (Owner, VisibleView)" appContext questionnaire2 reqAuthHeader 1

create_test_204 title appContext qtn authHeader docCount =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT (qtn ^. uuid)
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO TML.runMigration appContext
    runInContextIO QTN.runMigration appContext
    runInContextIO DOC.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findQuestionnaires appContext 2
    assertCountInDB findDocuments appContext docCount

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST when package can't be deleted" $
    -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT (questionnaire4 ^. uuid)
    let reqHeaders = reqHeadersT reqAuthHeader
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto = UserError _ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION
    let expBody = encode expDto
    -- AND: Prepare DB
    runInContextIO U.runMigration appContext
    runInContextIO TML.runMigration appContext
    runInContextIO QTN.runMigration appContext
    runInContextIO (insertQuestionnaire questionnaire4) appContext
    runInContextIO (insertQuestionnaire questionnaire4Upgraded) appContext
    runInContextIO (insertMigratorState nlQtnMigrationState) appContext
    runInContextIO DOC.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findQuestionnaires appContext 5
    assertCountInDB findDocuments appContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT (questionnaire3 ^. uuid)) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext questionnaire1
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" appContext questionnaire1
  create_test_403 "HTTP 403 NO CONTENT (Non-Owner, VisibleEdit)" appContext questionnaire3

create_test_403 title appContext qtn =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT (questionnaire1 ^. uuid)
    let reqHeaders = reqHeadersT reqNonAdminAuthHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Questionnaire"
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO TML.runMigration appContext
    runInContextIO QTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findQuestionnaires appContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
