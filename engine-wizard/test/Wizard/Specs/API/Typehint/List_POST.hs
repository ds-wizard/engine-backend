module Wizard.Specs.API.Typehint.List_POST
  ( list_post
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Database.Migration.Development.Typehint.Data.Typehints
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /typehints
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext = describe "POST /typehints" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/typehints"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = typehintRequest

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
    let expDto = [lifeScienceTypehint, mathematicalTypehint, legalTypehint]
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
