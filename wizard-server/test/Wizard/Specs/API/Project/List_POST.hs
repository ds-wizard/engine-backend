module Wizard.Specs.API.Project.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectCreateJM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Project

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/projects" $ do
    test_201 appContext
    test_400 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/projects"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project = project

reqBodyT project = encode (reqDtoT project)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 appContext "HTTP 201 CREATED (with token)" False project1Create [reqAuthHeader]
  create_test_201
    appContext
    "HTTP 201 CREATED (without token)"
    True
    (project1Create {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectCreateDTO)
    []

create_test_201 appContext title anonymousSharingEnabled project authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT project
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto =
            if anonymousSharingEnabled
              then project1Dto {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectDTO
              else project1Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO deleteProjects appContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing appContext anonymousSharingEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectCreateDtos resBody expDto
      -- AND: Find a result in DB
      (Right eventsInDB) <- runInContextIO (findProjectEventsByProjectUuid resBody.uuid) appContext
      if anonymousSharingEnabled
        then
          assertExistenceOfProjectInDB
            appContext
            ( project1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , sharing = AnyoneWithLinkEditProjectSharing
                , projectTags = []
                , permissions = []
                , creatorUuid = Nothing
                }
              :: Project
            )
            eventsInDB
        else do
          let aPermissions =
                [ (head project1.permissions)
                    { projectUuid = resBody.uuid
                    }
                  :: ProjectPerm
                ]
          assertExistenceOfProjectInDB
            appContext
            ( project1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , projectTags = []
                , permissions = aPermissions
                }
              :: Project
            )
            eventsInDB

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "packageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (reqBodyT project1Create) "PRJ_PERM"
