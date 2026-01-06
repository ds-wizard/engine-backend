module Wizard.Specs.API.Project.Version.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Version.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/projects/{projectUuid}/versions/{vUuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/projects/{projectUuid}/versions/{vUuid}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/projects/af984a75-56e3-49f8-b16f-d6b99599910a/versions/af984a75-56e3-49f8-b16f-dd016270ce7e"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = projectVersion1EditedChangeDto project1Uuid

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 20O OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = projectVersion1EditedList project1Uuid
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectVersionList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectVersionCreateDtos resBody expDto
      -- AND: Find a result in DB
      assertExistenceOfProjectVersionInDB appContext (projectVersion1Edited project1Uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/projects/00084a75-56e3-49f8-b16f-d6b99599910a/versions/bd6611c8-ea11-48ab-adaa-3ce51b66aae5"
    reqHeaders
    reqBody
    "project"
    [("uuid", "00084a75-56e3-49f8-b16f-d6b99599910a")]
