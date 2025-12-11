module Wizard.Specs.API.Project.Detail_Revert_Preview_POST (
  detail_revert_preview_POST,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects/{projectUuid}/revert/preview
-- ------------------------------------------------------------------------
detail_revert_preview_POST :: AppContext -> SpecWith ((), Application)
detail_revert_preview_POST appContext =
  describe "POST /wizard-api/projects/{projectUuid}/revert/preview" $ do
    test_200 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/projects/af984a75-56e3-49f8-b16f-d6b99599910a/revert/preview"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDto = projectVersion1RevertDto project1Uuid

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (logged user)" appContext True [reqAuthHeader]
  create_test_200 "HTTP 200 OK (anonymous)" appContext False []

create_test_200 title appContext showComments authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expProject = project1 {sharing = AnyoneWithLinkViewProjectSharing}
      let expProjectEvents = project1Events
      let expDto =
            if showComments
              then project1CtnRevertedDto
              else project1CtnRevertedDto {commentThreadsMap = M.empty}
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO PRJ.runMigration appContext
      runInContextIO (updateProjectByUuid expProject) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB appContext expProject expProjectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"
