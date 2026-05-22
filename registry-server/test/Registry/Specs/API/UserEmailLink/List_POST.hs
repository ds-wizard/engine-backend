module Registry.Specs.API.UserEmailLink.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Registry.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Registry.Model.UserEmailLink.UserEmailLinkType
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import Shared.Common.Model.Error.Error
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink

import Registry.Specs.API.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /user-email-links
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /user-email-links" $ do
    test_201 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/user-email-links"

reqHeaders = [reqCtHeader]

reqDto = forgottenTokenUserEmailLinkDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      userEmailLinkFromDb <- getFirstFromDB findUserEmailLinks appContext
      liftIO $ userEmailLinkFromDb.aType `shouldBe` reqDto.aType
      liftIO $ userEmailLinkFromDb.identity `shouldBe` orgGlobal.organizationId

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "type"
  it "HTTP 400 BAD REQUEST when email doesn't exist" $
    -- GIVEN: Prepare request
    do
      let reqDto = forgottenTokenUserEmailLinkDto {email = "non-existing@example.com"} :: UserEmailLinkDTO UserEmailLinkType
      let reqBody = encode reqDto
      -- Prepare expectation
      let expStatus = 400
      let expHeaders = resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__ORGANIZATION_EMAIL_ABSENCE "non-existing@example.com"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
