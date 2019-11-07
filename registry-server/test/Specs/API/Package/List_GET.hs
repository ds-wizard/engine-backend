module Specs.API.Package.List_GET
  ( list_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Audit.AuditEntryDAO
import Database.Migration.Development.Audit.Data.AuditEntries
import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages
import Model.Context.AppContext
import Service.Package.PackageMapper

import Specs.API.Audit.Common
import Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /packages
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext = describe "GET /packages" $ do test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/packages"

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
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto =
          [toSimpleDTO (toPackage globalPackage) orgDsw, toSimpleDTO (toPackage netherlandsPackageV2) orgNetherlands]
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
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto =
          [toSimpleDTO (toPackage globalPackage) orgDsw, toSimpleDTO (toPackage netherlandsPackageV2) orgNetherlands]
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfAuditEntryInDB appContext listPackagesAuditEntry
