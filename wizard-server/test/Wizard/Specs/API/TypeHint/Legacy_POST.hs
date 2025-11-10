module Wizard.Specs.API.TypeHint.Legacy_POST (
  legacy_POST,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import Wizard.Model.Context.AppContext

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
      runInContextIO KnowledgeModelPackage.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
