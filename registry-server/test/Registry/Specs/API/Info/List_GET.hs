module Registry.Specs.API.Info.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Api.Resource.Info.InfoJM ()
import Shared.Common.Database.Migration.Development.Info.Data.Infos
import qualified Shared.Component.Database.Migration.Development.Component.ComponentMigration as CMP_Migration

import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext = describe "GET /" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/"

reqHeaders = []

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
      let expDto = appInfo
      let expBody = encode expDto
      -- AND: Prepare DB
      runInContextIO CMP_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
