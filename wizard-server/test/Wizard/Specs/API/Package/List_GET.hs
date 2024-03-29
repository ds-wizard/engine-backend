module Wizard.Specs.API.Package.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Wizard.Api.Resource.Package.PackageSimpleDTO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageMapper
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/packages
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/packages" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  let expOrgRs = [globalRegistryOrganization, nlRegistryOrganization]
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/packages?sort=name,asc"
    ( Page
        "packages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] expOrgRs (toPackage germanyPackage)
        , toSimpleDTO' [globalRegistryPackage] expOrgRs (toPackage globalPackage)
        , toSimpleDTO' [nlRegistryPackage] expOrgRs (toPackage netherlandsPackageV2)
        ]
    )
  create_test_200
    "HTTP 200 OK (query - q)"
    appContext
    "/wizard-api/packages?q=Germany Knowledge Model"
    (Page "packages" (PageMetadata 20 1 1 0) [toSimpleDTO' [] expOrgRs (toPackage germanyPackage)])
  create_test_200
    "HTTP 200 OK (query - kmId)"
    appContext
    "/wizard-api/packages?kmId=core-nl"
    ( Page
        "packages"
        (PageMetadata 20 1 1 0)
        [toSimpleDTO' [nlRegistryPackage] expOrgRs (toPackage netherlandsPackageV2)]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/wizard-api/packages?q=Non-existing Knowledge Model"
    (Page "packages" (PageMetadata 20 0 0 0) ([] :: [PackageSimpleDTO]))
  create_test_200
    "HTTP 200 OK (outdated=false)"
    appContext
    "/wizard-api/packages?outdated=false&sort=name,asc"
    ( Page
        "packages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] expOrgRs (toPackage germanyPackage)
        , toSimpleDTO' [globalRegistryPackage] expOrgRs (toPackage globalPackage)
        , toSimpleDTO' [nlRegistryPackage] expOrgRs (toPackage netherlandsPackageV2)
        ]
    )
  create_test_200
    "HTTP 200 OK (outdated=true)"
    appContext
    "/wizard-api/packages?outdated=true"
    (Page "packages" (PageMetadata 20 0 0 0) ([] :: [PackageSimpleDTO]))

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO R_Migration.runMigration appContext
      runInContextIO PKG.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PM_READ_PERM"
