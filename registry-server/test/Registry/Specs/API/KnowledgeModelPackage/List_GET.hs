module Registry.Specs.API.KnowledgeModelPackage.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.Migration.Development.Audit.Data.AuditEntries
import Registry.Model.Context.AppContext
import Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

import Registry.Specs.API.Audit.Common
import Registry.Specs.API.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /knowledge-model-packages
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext = describe "GET /knowledge-model-packages" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/knowledge-model-packages"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK (Without Audit)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            [toSimpleDTO globalKmPackage orgGlobal, toSimpleDTO netherlandsKmPackageV2 orgNetherlands]
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findAuditEntries appContext 0
  it "HTTP 200 OK (With Audit)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqCtHeader, reqAdminAuthHeader] ++ reqStatisticsHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            [toSimpleDTO globalKmPackage orgGlobal, toSimpleDTO netherlandsKmPackageV2 orgNetherlands]
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfAuditEntryInDB appContext listPackagesAuditEntry
