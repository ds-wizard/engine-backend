module Wizard.Specs.API.Project.Detail_Settings_PUT (
  detail_settings_PUT,
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
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/projects/{projectUuid}/settings
-- ------------------------------------------------------------------------
detail_settings_PUT :: AppContext -> SpecWith ((), Application)
detail_settings_PUT appContext =
  describe "PUT /wizard-api/projects/{projectUuid}/settings" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/settings"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project =
  ProjectSettingsChangeDTO
    { name = project.name
    , description = project.description
    , projectTags = project.projectTags
    , documentTemplateId = project.documentTemplateId
    , formatUuid = project.formatUuid
    , isTemplate = project.isTemplate
    }

reqBodyT project = encode $ reqDtoT project

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    project1
    project1SettingsEdited
    project1Events
    project1Ctn
    []
    True
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Owner, VisibleView)"
    appContext
    project2
    project2SettingsEdited
    project2Events
    project2Ctn
    []
    False
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Non-Owner, Private, Sharing, Anonymous Enabled)"
    appContext
    project10
    project10EditedSettings
    project10Events
    project10Ctn
    [project10NikolaEditProjectPermDto]
    False
    [reqNonAdminAuthHeader]
    True

create_test_200 title appContext project projectEdited projectEvents projectContent permissions showComments authHeader anonymousEnabled =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ project.uuid
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = reqDtoT projectEdited
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (insertProject project10) appContext
      runInContextIO (insertProjectEvents project10Events) appContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing appContext anonymousEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB appContext projectEdited projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT project3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext =
  createAuthTest reqMethod (reqUrlT project3.uuid) [reqCtHeader] (reqBodyT project1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest
    appContext
    reqMethod
    (reqUrlT project3.uuid)
    [reqCtHeader]
    (reqBodyT project1)
    "PRJ_PERM"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    project1
    project1SettingsEdited
    "Administrate Project"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    project2
    project2SettingsEdited
    "Administrate Project"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private, Sharing, Anonymous Disabled)"
    appContext
    project10
    project10EditedSettings
    "Administrate Project"

create_test_403 title appContext project projectEdited reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ project.uuid
      let reqHeaders = reqHeadersT [reqNonAdminAuthHeader]
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (insertProject project10) appContext
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/settings"
    (reqHeadersT [reqAuthHeader])
    (reqBodyT project1)
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
