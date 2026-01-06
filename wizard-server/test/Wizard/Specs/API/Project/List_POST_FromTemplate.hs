module Wizard.Specs.API.Project.List_POST_FromTemplate (
  list_POST_fromTemplate,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Wizard.Api.Resource.Project.ProjectCreateJM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import Wizard.Model.Tenant.Config.TenantConfig hiding (request)
import Wizard.Service.Tenant.Config.ConfigService

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects?fromTemplate=true
-- ------------------------------------------------------------------------
list_POST_fromTemplate :: AppContext -> SpecWith ((), Application)
list_POST_fromTemplate appContext =
  describe "POST /wizard-api/projects/from-template" $ do
    test_201 appContext
    test_400 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/projects/from-template"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT projectTemplateUuid name =
  ProjectCreateFromTemplateDTO
    { name = name
    , projectUuid = projectTemplateUuid
    }

reqBodyT projectTemplateUuid name = encode (reqDtoT projectTemplateUuid name)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project1.uuid project11.name
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = project11Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectCreateFromTemplateDtos resBody expDto
      -- AND: Find a result in DB
      assertCountInDB findProjects appContext 4

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST (projectCreation: CustomProjectCreation)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project2.uuid project11.name
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Project Template"
      let expBody = encode expDto
      -- AND: Change tenantConfig
      (Right tcProject) <- runInContextIO getCurrentTenantConfigProject appContext
      let tcProjectUpdated = tcProject {projectCreation = CustomProjectCreation}
      runInContextIO (modifyTenantConfigProject tcProjectUpdated) appContext
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertCountInDB findProjects appContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  it "HTTP 403 FORBIDDEN (isTemplate: False)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project2.uuid project11.name
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Project Template"
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertCountInDB findProjects appContext 3
