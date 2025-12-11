module Wizard.Specs.API.Project.List_GET (
  list_GET,
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
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/projects" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/projects"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&page=1&size=1"
    reqAuthHeader
    (Page "projects" (PageMetadata 1 6 6 1) [project14Dto])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&q=pri"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project1Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids)"
    appContext
    (BS.pack $ "/wizard-api/projects?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project1Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, or)"
    appContext
    ( BS.pack $
        "/wizard-api/projects?sort=uuid,asc&userUuidsOp=or&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project1Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, and)"
    appContext
    ( BS.pack $
        "/wizard-api/projects?sort=uuid,asc&userUuidsOp=and&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - true)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=true"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project14Dto, project1Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - false)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=false"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project3Dto, project15Dto, project2Dto])
  create_test_200
    "HTTP 200 OK (Admin - isMigrating - true)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isMigrating=true"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Admin - isMigrating - false)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isMigrating=false"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project3Dto, project14Dto, project1Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&projectTags=projectTag1"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 4 1 0)
        [project14Dto, project1Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, or)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&projectTagsOp=or&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 4 1 0)
        [project14Dto, project1Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, and)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&projectTagsOp=and&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project2Dto])
  create_test_200
    "HTTP 200 OK (Admin - knowledgePackage)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam:core-amsterdam:all"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project14Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    appContext
    "/wizard-api/projects?sort=uuid,asc"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project3Dto, project14Dto, project1Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    appContext
    "/wizard-api/projects?sort=updatedAt,desc"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project15Dto, project3Dto, project14Dto, project1Dto, project12Dto, project2Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin)"
    appContext
    "/wizard-api/projects?sort=uuid,asc"
    reqNonAdminAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 5 1 0)
        [project3Dto, project14Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin - query)"
    appContext
    "/wizard-api/projects?q=pri"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query users)"
    appContext
    (BS.pack $ "/wizard-api/projects?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - projectTags)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&projectTags=projectTag1"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project14Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - knowledgeModelPackage)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam:core-amsterdam:all"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project14Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - true)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=true"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project14Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - false)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=false"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project3Dto, project15Dto, project2Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isMigrating - true)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isMigrating=true"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Non-Admin - isMigrating - false)"
    appContext
    "/wizard-api/projects?sort=uuid,asc&isMigrating=false"
    reqNonAdminAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 5 1 0)
        [project3Dto, project14Dto, project15Dto, project2Dto, project12Dto]
    )

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
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (insertPackage amsterdamKmPackage) appContext
      runInContextIO (insertProject project12) appContext
      runInContextIO (insertProject project14) appContext
      runInContextIO (insertProject project15) appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PRJ_PERM"
