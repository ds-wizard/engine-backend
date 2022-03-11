module Wizard.Specs.API.Template.File.Detail_PUT
  ( detail_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Resource.Template.TemplateChangeJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.File.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /templates/{tmlId}/files/{fileUuid}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith ((), Application)
detail_put appContext =
  describe "PUT /templates/{tmlId}/files/{fileUuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/templates/global:questionnaire-report:1.0.0/files/7f83f7ce-4096-49a5-88d1-bd509bf72a9b"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = templateFileDefaultHtmlEditedChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = create_test_200 "HTTP 200 OK" appContext reqAuthHeader

create_test_200 title appContext reqAuthHeader =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = templateFileDefaultHtmlEdited
     -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemplateFile)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareTemplateFileDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfTemplateFileInDB appContext templateFileDefaultHtmlEdited (commonWizardTemplate ^. tId)

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
  createNotFoundTest'
    reqMethod
    "/templates/global:questionnaire-report:1.0.0/files/deab6c38-aeac-4b17-a501-4365a0a70176"
    (reqHeadersT reqAuthHeader)
    reqBody
    "template_file"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
