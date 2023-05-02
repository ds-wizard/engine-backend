module Wizard.Specs.API.BookReference.Detail_GET (
  detail_get,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Api.Resource.BookReference.BookReferenceJM ()
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceMigration as BR
import Wizard.Database.Migration.Development.BookReference.Data.BookReferences
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /book-references/{brShortUuid}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith ((), Application)
detail_get appContext =
  describe "GET /book-references/{brShortUuid}" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/book-references/bvq"

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
      let expDto = bookReferenceBvq
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO BR.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/book-references/nonExistingShortUuid"
    reqHeaders
    reqBody
    "book_reference"
    [("short_uuid", "nonExistingShortUuid")]
