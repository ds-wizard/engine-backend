module Wizard.Specs.API.Template.List_Suggestions_GET
  ( list_suggestions_GET
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Template.TemplateJM ()
import Shared.Service.Template.TemplateMapper
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /templates/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /templates/suggestions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/templates/suggestions"

reqHeadersT reqAuthHeader = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/templates/suggestions"
    reqAuthHeader
    (Page "templates" (PageMetadata 20 1 1 0) [toSuggestionDTO commonWizardTemplateEdited])
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/templates/suggestions?q=Questionnaire Report"
    reqAuthHeader
    (Page "templates" (PageMetadata 20 1 1 0) [toSuggestionDTO commonWizardTemplateEdited])
  create_test_200
    "HTTP 200 OK (query 'pkgId')"
    appContext
    "/templates/suggestions?pkgId=global:core:1.0.0"
    reqAuthHeader
    (Page "templates" (PageMetadata 20 1 1 0) [toSuggestionDTO commonWizardTemplateEdited])
  create_test_200
    "HTTP 200 OK (query 'pkgId' - no templates)"
    appContext
    "/templates/suggestions?pkgId=org.nl:core-nl:1.0.0"
    reqAuthHeader
    (Page "templates" (PageMetadata 20 0 0 0) [])
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/templates/suggestions?q=Non-existing Questionnaire Report"
    reqAuthHeader
    (Page "templates" (PageMetadata 20 0 0 0) [])

create_test_200 title appContext reqUrl reqAuthHeader expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO (updateTemplateById commonWizardTemplateEdited) appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "DMP_PERM"
