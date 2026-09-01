module Wizard.Specs.API.Project.Migration.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateJM ()
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects/{projectUuid}/migrations
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/projects/{projectUuid}/migrations" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/migrations"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDto = projectMigrationCreateDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext project4 project4Events
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView)" appContext project4VisibleView project4VisibleViewEvents
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" appContext project4VisibleEdit project4VisibleEditEvents

create_test_200 title appContext project projectEvents =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCorsHeadersPlain
      -- AND: Prepare database
      runInContextIO TML.runMigration appContext
      runInContextIO (insertProject project) appContext
      runInContextIO (insertProjectEvents projectEvents) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDetailQuestionnaireDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.uuid `shouldBe` project.uuid
      liftIO $ resBody.knowledgeModelPackage.uuid `shouldBe` netherlandsKmPackageV2.uuid
      liftIO $ resBody.knowledgeModel `shouldBe` km1NetherlandsV2
      liftIO $ resBody.phaseUuid `shouldBe` project4Ctn.phaseUuid
      liftIO $ resBody.replies `shouldBe` M.empty
      -- AND: Find a result in DB
      assertCountInDB findProjects appContext 1
      eProjectFromDb <- runInContextIO (findProjectByUuid project.uuid) appContext
      liftIO $ isRight eProjectFromDb `shouldBe` True
      let (Right projectFromDb) = eProjectFromDb
      liftIO $ projectFromDb.knowledgeModelPackageUuid `shouldBe` netherlandsKmPackageV2.uuid
      liftIO $ projectFromDb.selectedQuestionTagUuids `shouldBe` []
      liftIO $ projectFromDb.squashed `shouldBe` False
      eProjectEventsFromDb <- runInContextIO (findProjectEventsByProjectUuid project.uuid) appContext
      liftIO $ isRight eProjectEventsFromDb `shouldBe` True
      let (Right projectEventsFromDb) = eProjectEventsFromDb
      liftIO $ projectEventsFromDb `shouldBe` projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT project4.uuid) "targetKnowledgeModelPackageUuid"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT project4.uuid) [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext project1 "Migrate Project"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" appContext project2 "Migrate Project"

create_test_403 title appContext project reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT reqNonAdminAuthHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
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
    (reqUrlT project4.uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "project"
    [("uuid", "57250a07-a663-4ff3-ac1f-16530f2c1bfe")]
