module Wizard.Specs.API.TypeHint.Legacy_POST (
  legacy_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import Wizard.Model.Context.AppContext
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/type-hints-legacy
-- ------------------------------------------------------------------------
legacy_POST :: AppContext -> SpecWith ((), Application)
legacy_POST appContext = describe "POST /wizard-api/type-hints-legacy" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/type-hints-legacy"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = typeHintLegacyRequest

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [lifeScienceLegacyTypeHint, mathematicalLegacyTypeHint, legalLegacyTypeHint]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO PKG.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
