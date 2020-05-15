module Wizard.Specs.API.Template.List_GET
  ( list_get
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
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Context.AppContext
import Wizard.Service.Template.TemplateMapper

import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /templates
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext =
  describe "GET /templates" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/templates"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = [toDTO (fmap SPM.toPackage [globalPackage, netherlandsPackageV2]) commonWizardTemplate]
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
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest (appContext ^. serverConfig) reqMethod reqUrl [reqCtHeader] reqBody "DMP_PERM"
