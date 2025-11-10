module Wizard.Specs.API.KnowledgeModelPackage.List_GET (
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
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/knowledge-model-packages"

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
    "/wizard-api/knowledge-model-packages?sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] expOrgRs germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] expOrgRs globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] expOrgRs netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (query - q)"
    appContext
    "/wizard-api/knowledge-model-packages?q=Germany Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSimpleDTO' [] expOrgRs germanyKmPackage])
  create_test_200
    "HTTP 200 OK (query - kmId)"
    appContext
    "/wizard-api/knowledge-model-packages?kmId=core-nl"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 1 1 0)
        [toSimpleDTO' [nlRegistryPackage] expOrgRs netherlandsKmPackageV2]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/wizard-api/knowledge-model-packages?q=Non-existing Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))
  create_test_200
    "HTTP 200 OK (outdated=false)"
    appContext
    "/wizard-api/knowledge-model-packages?outdated=false&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] expOrgRs germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] expOrgRs globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] expOrgRs netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (outdated=true)"
    appContext
    "/wizard-api/knowledge-model-packages?outdated=true"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO R_Migration.runMigration appContext
      runInContextIO KnowledgeModelPackage.runMigration appContext
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
