module Wizard.Specs.API.Migration.KnowledgeModel.List_Current_Conflict_POST (
  list_current_conflict_POST,
) where

import Data.Aeson (encode)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Model.Error.Error
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations
import Wizard.Localization.Messages.Public
import Wizard.Model.Branch.BranchList
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Migration.KnowledgeModel.MigratorService

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Migration.KnowledgeModel.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /branches/{branchId}/migrations/current/conflict
-- ------------------------------------------------------------------------
list_current_conflict_POST :: AppContext -> SpecWith ((), Application)
list_current_conflict_POST appContext =
  describe "POST /branches/{branchId}/migrations/current/conflict" $ do
    test_204 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current/conflict"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = migratorConflict

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = ""
      -- AND: Prepare database
      runMigrationWithFullDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertStateOfMigrationInDB appContext migratorState CompletedState

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "targetPackageId"
  it "HTTP 400 BAD REQUEST when originalEventUuid doesn't match with current target event" $
    -- GIVEN: Prepare request
    do
      let reqDtoEdited = reqDto {originalEventUuid = fromJust . U.fromString $ "30ac5193-5685-41b1-86d7-ab0b356c516a"}
      let reqBody = encode reqDtoEdited
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when edit migration action has to provide target event" $
    -- GIVEN: Prepare request
    do
      let reqDtoEdited = reqDto {event = Nothing}
      let reqBody = encode reqDtoEdited
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_MIGRATION_KM__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when you can't solve conflicts because Migration state isn't in conflict state" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB appContext
      runInContextIO (solveConflictAndMigrate amsterdamBranchList.uuid reqDto) appContext
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
test_404 appContext =
  createNotFoundTest'
    reqMethod
    reqUrl
    reqHeaders
    reqBody
    "knowledge_model_migration"
    [("branch_uuid", "6474b24b-262b-42b1-9451-008e8363f2b6")]
