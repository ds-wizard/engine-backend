module Registry.Specs.API.ActionKey.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.ActionKey.ActionKeyJM ()
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Localization.Messages.Public
import Registry.Model.ActionKey.ActionKeyType
import Registry.Model.Context.AppContext
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyJM ()
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Model.Error.Error

import Registry.Specs.API.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /action-keys
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /action-keys" $ do
    test_201 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/action-keys"

reqHeaders = [reqCtHeader]

reqDto = forgTokActionKeyDto

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
      actionKeyFromDb <- getFirstFromDB findActionKeys appContext
      liftIO $ actionKeyFromDb.aType `shouldBe` reqDto.aType
      liftIO $ actionKeyFromDb.identity `shouldBe` orgGlobal.organizationId

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "type"
  it "HTTP 400 BAD REQUEST when email doesn't exist" $
    -- GIVEN: Prepare request
    do
      let reqDto = forgTokActionKeyDto {email = "non-existing@example.com"} :: ActionKeyDTO ActionKeyType
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
