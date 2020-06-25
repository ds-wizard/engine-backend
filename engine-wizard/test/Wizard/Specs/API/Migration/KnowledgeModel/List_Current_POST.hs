module Wizard.Specs.API.Migration.KnowledgeModel.List_Current_POST
  ( list_current_POST
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService
import qualified Wizard.Service.User.UserMapper as U_Mapper

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Migration.KnowledgeModel.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /branches/{branchId}/migrations/current
-- ------------------------------------------------------------------------
list_current_POST :: AppContext -> SpecWith ((), Application)
list_current_POST appContext =
  describe "POST /branches/{branchId}/migrations/current" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"

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
    let expDto = createUserError _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS
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
    let reqDto = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = "org.nl:core-nl:0.9.0"}
    let reqBody = encode reqDto
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createUserError _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER
    let expBody = encode expDto
    -- AND: Prepare database
    runMigrationWithEmptyDB appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when branch has to have a previous package" $
    -- AND: Prepare expectation
   do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createUserError _ERROR_VALIDATION__BRANCH_PREVIOUS_PKG_ABSENCE
    let expBody = encode expDto
    -- AND: Prepare database
    let branch = amsterdamBranchCreate {_branchCreateDTOPreviousPackageId = Nothing}
    let branchUuid = amsterdamBranch ^. uuid
    let timestamp = amsterdamBranch ^. createdAt
    let user = U_Mapper.toDTO userAlbert
    runInContextIO (createBranchWithParams branchUuid timestamp user branch) appContext
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
  createNotFoundTest reqMethod reqUrl reqHeaders reqBody "branch" "6474b24b-262b-42b1-9451-008e8363f2b6"
  it "HTTP 404 NOT FOUND when target previous package doesn’t exist" $
    -- GIVEN: Prepare expectation
   do
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createNotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "package" "org.nl:core-nl:2.0.0")
    let expBody = encode expDto
    -- AND: Prepare database
    runMigrationWithEmptyDB appContext
    runInContextIO (deletePackageById (netherlandsPackageV2 ^. pId)) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
