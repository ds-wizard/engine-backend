module Wizard.Specs.API.ProjectCommentThread.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Comment.ProjectComment
import Wizard.Model.Project.Comment.ProjectCommentThreadAssigned
import Wizard.Model.Project.Project
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/project-comment-threads
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/project-comment-threads" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/project-comment-threads?sort=updatedAt,desc"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO (insertProject project1) appContext
      -- AND: Create thread without assignee
      thread1 <- liftIO . create_cmtQ1_t1 $ project1.uuid
      comment1_1 <- liftIO . create_cmtQ1_t1_1 $ thread1.uuid
      comment1_2 <- liftIO . create_cmtQ1_t1_2 $ thread1.uuid
      runInContextIO (insertProjectCommentThread thread1) appContext
      runInContextIO (insertProjectComment comment1_1) appContext
      runInContextIO (insertProjectComment comment1_2) appContext
      -- AND: Create thread with assignee
      thread2 <- liftIO . create_cmtQ1_t1 $ project1.uuid
      comment2_1 <- liftIO . create_cmtQ1_t1_1 $ thread2.uuid
      comment2_2 <- liftIO . create_cmtQ1_t1_2 $ thread2.uuid
      runInContextIO (insertProjectCommentThread (thread2 {assignedTo = Just userAlbert.uuid})) appContext
      runInContextIO (insertProjectComment comment2_1) appContext
      runInContextIO (insertProjectComment comment2_2) appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = Page "projectCommentThreads" (PageMetadata 20 1 1 0) [cmtAssigned {commentThreadUuid = thread2.uuid}]
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "PRJ_PERM"
