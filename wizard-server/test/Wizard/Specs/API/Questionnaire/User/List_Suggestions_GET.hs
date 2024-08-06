module Wizard.Specs.API.Questionnaire.User.List_Suggestions_GET (
  list_suggestions_GET,
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
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.User.UserMapper
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaires/{qtnUuid}/users/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/questionnaires/{qtnUuid}/users/suggestions" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid query = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/users/suggestions?sort=uuid,asc" ++ query

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    questionnaire1
    [reqAuthHeader]
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestionDTO . toSuggestion) [userAlbert]))
  create_test_200
    "HTTP 200 OK (Commentator)"
    appContext
    (questionnaire13 {visibility = PrivateQuestionnaire})
    [reqNonAdminAuthHeader]
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola]))
  create_test_200
    "HTTP 200 OK (Non-Commentator, VisibleComment)"
    appContext
    questionnaire13
    [reqIsaacAuthTokenHeader]
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (questionnaire13 {sharing = AnyoneWithLinkCommentQuestionnaire})
    []
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    questionnaire3
    [reqNonAdminAuthHeader]
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    appContext
    questionnaire10
    []
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestionDTO . toSuggestion) [userNikola, userIsaac, userAlbert]))

create_test_200 title appContext qtn authHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire qtn) appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire")
  create_test_403
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
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
      let reqUrl = reqUrlT qtn.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire qtn) appContext
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
    "/wizard-api/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/users/suggestions"
    [reqHeadersT reqAuthHeader]
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
