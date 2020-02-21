module Wizard.Specs.API.Document.List_GET
  ( list_GET
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import Wizard.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Document.DocumentMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /documents
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith Application
list_GET appContext =
  describe "GET /documents" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/documents"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 CREATED (Admin)"
    appContext
    [ toDTO doc1 (Just $ QTN_Mapper.toSimpleDTO questionnaire1 germanyPackage QSDefault) commonWizardTemplate
    , toDTO doc2 (Just $ QTN_Mapper.toSimpleDTO questionnaire2 germanyPackage QSDefault) commonWizardTemplate
    , toDTO doc3 (Just $ QTN_Mapper.toSimpleDTO questionnaire2 germanyPackage QSDefault) commonWizardTemplate
    ]
    reqAuthHeader
  create_test_200
    "HTTP 200 CREATED (Non-Admin)"
    appContext
    [ toDTO doc1 Nothing commonWizardTemplate
    , toDTO doc2 (Just $ QTN_Mapper.toSimpleDTO questionnaire2 germanyPackage QSDefault) commonWizardTemplate
    ]
    reqNonAdminAuthHeader

create_test_200 title appContext expDto authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest (appContext ^. applicationConfig) reqMethod reqUrl [reqCtHeader] reqBody "DMP_PERM"
