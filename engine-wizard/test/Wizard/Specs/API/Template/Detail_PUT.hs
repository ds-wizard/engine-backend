module Wizard.Specs.API.Template.Detail_PUT
  ( detail_put
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import Wizard.Api.Resource.Template.TemplateChangeJM ()
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /templates/{tmlId}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith ((), Application)
detail_put appContext =
  describe "PUT /templates/{tmlId}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/templates/dsw:default-template:1.0.0"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = commonWizardTemplateEditedChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (user token)" appContext reqAuthHeader
  create_test_200 "HTTP 200 OK (service token)" appContext reqServiceHeader

create_test_200 title appContext reqAuthHeader =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = commonWizardTemplateEdited
     -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Template)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareTemplateDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfTemplateInDB appContext commonWizardTemplateEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "TML_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/templates/deab6c38-aeac-4b17-a501-4365a0a70176"
    (reqHeadersT reqAuthHeader)
    reqBody
    "template"
    "deab6c38-aeac-4b17-a501-4365a0a70176"
