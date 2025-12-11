module Wizard.Specs.API.Project.Detail_Documents_GET (
  detail_documents_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Error.Error
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Project.Project
import Wizard.Model.User.User
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Document.DocumentMapper
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/documents
-- ------------------------------------------------------------------------
detail_documents_GET :: AppContext -> SpecWith ((), Application)
detail_documents_GET appContext =
  describe "GET /wizard-api/projects/{projectUuid}/documents" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/documents?sort=name,asc"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 CREATED (Owner)" appContext [reqAuthHeader]
  create_test_200 "HTTP 200 CREATED (Non-Owner)" appContext [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 CREATED (Anonymous)" appContext []

create_test_200 title appContext authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project6.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      let doc1' = doc1 {projectUuid = Just project6.uuid, projectEventUuid = Just . getUuid $ slble_rQ1' project6.uuid}
      let doc2' = doc2 {projectUuid = Just project6.uuid, projectEventUuid = Just . getUuid $ slble_rQ1' project6.uuid, createdBy = Just userIsaac.uuid}
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO PRJ_Migration.runMigration appContext
      runInContextIO (insertProject project6) appContext
      runInContextIO (insertProjectEvents project6Events) appContext
      runInContextIO (traverse_ insertProjectVersion project6Versions) appContext
      runInContextIO deleteDocuments appContext
      runInContextIO removeDocumentContents appContext
      runInContextIO (insertDocument doc1') appContext
      runInContextIO (insertDocument doc2') appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            Page
              "documents"
              (PageMetadata 20 2 1 0)
              [ toDTOWithDocTemplate doc1' project6 (Just "Version 1") []
              , toDTOWithDocTemplate doc2' project6 (Just "Version 1") []
              ]
      let expBody = encode (fmap (\x -> x wizardDocumentTemplate formatJsonSimple) expDto)
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
    "HTTP 403 FORBIDDEN (Anonymous, VisibleEdit)"
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
      runInContextIO TML_Migration.runMigration appContext
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/documents"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
