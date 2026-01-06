module Wizard.Specs.API.Project.Detail_Questionnaire_GET (
  detail_questionnaire_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Comment.ProjectComment
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/questionnaire
-- ------------------------------------------------------------------------
detail_questionnaire_GET :: AppContext -> SpecWith ((), Application)
detail_questionnaire_GET appContext =
  describe "GET /wizard-api/projects/{projectUuid}/questionnaire" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/questionnaire"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    project1
    project1Events
    project1Ctn
    True
    [reqAuthHeader]
    [project1AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    project2
    project2Events
    (project2Ctn {labels = M.empty} :: ProjectContent)
    False
    [reqNonAdminAuthHeader]
    [project2AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Commenter)"
    appContext
    (project13 {visibility = PrivateProjectVisibility})
    project13Events
    (project13Ctn {labels = M.empty} :: ProjectContent)
    True
    [reqNonAdminAuthHeader]
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    appContext
    project13
    project13Events
    (project13Ctn {labels = M.empty} :: ProjectContent)
    True
    [reqIsaacAuthTokenHeader]
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (project13 {sharing = AnyoneWithLinkCommentProjectSharing})
    project13Events
    (project13Ctn {labels = M.empty} :: ProjectContent)
    True
    []
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    appContext
    project7
    project7Events
    (project7Ctn {labels = M.empty} :: ProjectContent)
    False
    []
    [project7AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    project3
    project3Events
    project3Ctn
    True
    [reqNonAdminAuthHeader]
    []
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing)" appContext project10 project10Events project10Ctn True [] []

create_test_200 title appContext project projectEvents projectContent showComments authHeader permissions =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      thread1 <- liftIO . create_cmtQ1_t1 $ project.uuid
      comment1 <- liftIO . create_cmtQ1_t1_1 $ thread1.uuid
      comment2 <- liftIO . create_cmtQ1_t1_2 $ thread1.uuid
      runInContextIO (insertProject project) appContext
      runInContextIO (insertProjectEvents projectEvents) appContext
      runInContextIO (insertProjectCommentThread thread1) appContext
      runInContextIO (insertProjectComment comment1) appContext
      runInContextIO (insertProjectComment comment2) appContext
      let unresolvedCommentCounts =
            if showComments
              then M.fromList [(cmtQ1_path, M.fromList [(thread1.uuid, 2)])]
              else M.empty
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            ProjectDetailQuestionnaireDTO
              { uuid = project.uuid
              , name = project.name
              , visibility = project.visibility
              , sharing = project.sharing
              , knowledgeModelPackageId = project.knowledgeModelPackageId
              , selectedQuestionTagUuids = project.selectedQuestionTagUuids
              , isTemplate = project.isTemplate
              , knowledgeModel = km1WithQ4
              , replies = fReplies
              , labels = projectContent.labels
              , phaseUuid = projectContent.phaseUuid
              , migrationUuid = Nothing
              , permissions = permissions
              , files = []
              , unresolvedCommentCounts = unresolvedCommentCounts
              , resolvedCommentCounts = M.empty
              , projectActionsAvailable = 0
              , projectImportersAvailable = 0
              , fileCount = 0
              }
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
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    project3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/questionnaire"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
