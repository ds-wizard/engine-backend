module Wizard.Specs.API.Document.List_POST
  ( list_POST
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentCreateJM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.API.Document.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /documents
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /documents" $ do
    test_201 appContext
    test_400_invalid_json appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/documents"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDtoT doc =
  DocumentCreateDTO
    { _documentCreateDTOName = doc ^. name
    , _documentCreateDTOQuestionnaireUuid = doc ^. questionnaireUuid
    , _documentCreateDTOTemplateUuid = doc ^. templateUuid
    , _documentCreateDTOFormatUuid = doc ^. formatUuid
    }

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (Admin)" appContext doc1 reqAuthHeader
  create_test_201 "HTTP 201 CREATED (Non-Admin)" appContext doc2 reqNonAdminAuthHeader

create_test_201 title appContext doc authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
    let reqDto = reqDtoT doc2
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO deleteDocuments appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, DocumentDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareDocumentDtos resBody reqDto
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 1
    assertExistenceOfDocumentInDB appContext reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common KM" } |] "kmId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] (encode $ reqDtoT doc1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest (appContext ^. serverConfig) reqMethod reqUrl [reqCtHeader] (encode $ reqDtoT doc1) "DMP_PERM"
  it "HTTP 403 FORBIDDEN - Qtn is not accessible for user" $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT reqNonAdminAuthHeader
    let reqDto = reqDtoT doc1
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO deleteDocuments appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, DocumentDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 0
