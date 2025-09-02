module Registry.Specs.API.DocumentTemplate.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Registry.Model.Context.AppContext
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()

import Registry.Specs.Common
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /document-templates
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext = describe "GET /document-templates" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/document-templates"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK" appContext "/document-templates" [wizardDocumentTemplateSimpleDTO]
  create_test_200 "HTTP 200 OK (metamodelVersion=99.0)" appContext "/document-templates?metamodelVersion=99.0" [wizardDocumentTemplateSimpleDTO]
  create_test_200 "HTTP 200 OK (metamodelVersion=10.0)" appContext "/document-templates?metamodelVersion=10.0" ([] :: [DocumentTemplateSimpleDTO])

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
