module Wizard.Specs.API.Migration.KnowledgeModel.List_Current_POST (
  list_current_POST,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.Tenant.Tenant
import Wizard.Service.KnowledgeModel.Editor.EditorService
import qualified Wizard.Service.User.UserMapper as U_Mapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Migration.KnowledgeModel.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-editors/{uuid}/migrations/current
-- ------------------------------------------------------------------------
list_current_POST :: AppContext -> SpecWith ((), Application)
list_current_POST appContext =
  describe "POST /wizard-api/knowledge-model-editors/{uuid}/migrations/current" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = migratorStateCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = migratorState
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithEmptyDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findMigratorStates appContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "targetPackageId"
  it "HTTP 400 BAD REQUEST when migration is already created" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when target Package is not higher than current one" $
    -- GIVEN: Prepare request
    do
      let reqDto = KnowledgeModelMigrationCreateDTO {targetPackageId = "org.nl:core-nl:0.9.0"}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithEmptyDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when KM editor has to have a previous package" $
    -- AND: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__KM_EDITOR_PREVIOUS_PKG_ABSENCE
      let expBody = encode expDto
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditorCreate {previousPackageId = Nothing} :: KnowledgeModelEditorCreateDTO
      let editorUuid = amsterdamKnowledgeModelEditorList.uuid
      let timestamp = amsterdamKnowledgeModelEditorList.createdAt
      let user = U_Mapper.toDTO userAlbert
      runInContextIO (createEditorWithParams editorUuid timestamp user editor) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_UPGRADE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = do
  createNotFoundTest' reqMethod reqUrl reqHeaders reqBody "knowledge_model_editor" [("uuid", "6474b24b-262b-42b1-9451-008e8363f2b6")]
  it "HTTP 404 NOT FOUND when target previous package doesn’t exist" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "knowledge_model_package"
                  [("tenant_uuid", U.toString defaultTenant.uuid), ("id", "org.nl:core-nl:2.0.0")]
              )
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithEmptyDB appContext
      runInContextIO (deletePackageById netherlandsKmPackageV2.pId) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
