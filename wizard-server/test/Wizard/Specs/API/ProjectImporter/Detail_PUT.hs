module Wizard.Specs.API.ProjectImporter.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import qualified Wizard.Database.Migration.Development.Project.ProjectImporterMigration as ProjectImporterMigration
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Importer.ProjectImporter
import Wizard.Service.Project.Importer.ProjectImporterMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.ProjectImporter.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/project-importers/{id}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/project-importers/{id}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/project-importers/global:project-importer-bio:3.0.0"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = toChangeDTO projectImporterBio3Edited

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
      let expDto = toDTO projectImporterBio3Edited
      let expBody = encode expDto
      let expType (a :: ProjectImporterDTO) = a
      -- AND: Run migrations
      runInContextIO ProjectImporterMigration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, ProjectImporterDTO)
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfProjectImporterInDB appContext projectImporterBio3Edited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PRJ_IMPORTER_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/project-importers/deab6c38-aeac-4b17-a501-4365a0a70176"
    (reqHeadersT reqAuthHeader)
    reqBody
    "project_importer"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
