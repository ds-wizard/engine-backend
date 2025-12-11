module Wizard.Specs.API.Project.Detail_Revert_POST (
  detail_revert_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Version.ProjectVersion

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.API.Project.Version.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects/{projectUuid}/revert
-- ------------------------------------------------------------------------
detail_revert_POST :: AppContext -> SpecWith ((), Application)
detail_revert_POST appContext =
  describe "POST /wizard-api/projects/{projectUuid}/revert" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/projects/af984a75-56e3-49f8-b16f-d6b99599910a/revert"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = projectVersion1RevertDto project1Uuid

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = project1CtnRevertedDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB appContext project1 [sre_rQ1' project1Uuid, sre_rQ2' project1Uuid]
      assertAbsenceOfProjectVersionInDB appContext (projectVersion1 project1Uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
