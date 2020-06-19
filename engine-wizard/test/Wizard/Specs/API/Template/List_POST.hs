module Wizard.Specs.API.Template.List_POST
  ( list_post
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
import Wizard.Database.DAO.Template.TemplateDAO
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Template.TemplateMapper

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /templates
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext =
  describe "POST /templates" $ do
    test_201 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/templates"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = toChangeDTO anotherWizardTemplate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (user token)" appContext reqAuthHeader
  create_test_201 "HTTP 201 CREATED (service token)" appContext reqServiceHeader

create_test_201 title appContext reqAuthHeader =
  it title $
       -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = anotherWizardTemplate
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
    assertCountInDB findTemplates appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "TML_PERM"
