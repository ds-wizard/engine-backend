module Wizard.Specs.API.Common where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Servant (serve)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Bootstrap.Web
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Middleware.LoggingMiddleware
import Wizard.Api.Web
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.User
import Wizard.Service.User.UserService
import WizardLib.Public.Model.User.UserToken

import SharedTest.Specs.API.Common
import Wizard.Specs.Common

startWebApp :: BaseContext -> AppContext -> IO Application
startWebApp baseContext appContext = do
  let config = appContext.serverConfig
  let webPort = config.general.serverPort
  let env = config.general.environment
  return $ runMiddleware env loggingMiddleware $ serve webApi (webServer baseContext)

reqAuthToken :: String
reqAuthToken = albertToken.value

reqAuthHeader :: Header
reqAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ reqAuthToken)

reqNonAdminAuthToken :: String
reqNonAdminAuthToken = nikolaToken.value

reqNonAdminAuthHeader :: Header
reqNonAdminAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ reqNonAdminAuthToken)

reqIsaacAuthToken :: String
reqIsaacAuthToken = isaacToken.value

reqIsaacAuthTokenHeader :: Header
reqIsaacAuthTokenHeader = ("Authorization", BS.pack $ "Bearer " ++ reqIsaacAuthToken)

userWithoutPerm :: ServerConfig -> String -> User
userWithoutPerm serverConfig perm =
  let allPerms = getPermissionForRole serverConfig _USER_ROLE_ADMIN
   in userAlbert {permissions = L.delete perm allPerms}

createInvalidJsonTest reqMethod reqUrl missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqBody = BSL.pack "{}"
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeaderUtf8 : resCorsHeaders
    let expDto = object ["status" .= 400, "message" .= "Problem in deserialization of JSON"]
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

createNoPermissionTest appContext reqMethod reqUrl otherHeaders reqBody missingPerm =
  it "HTTP 403 FORBIDDEN - no required permission" $
    -- GIVEN: Prepare request
    do
      let user = userWithoutPerm appContext.serverConfig missingPerm
      runInContextIO (updateUserByUuid user) appContext
      let reqHeaders = reqAuthHeader : otherHeaders
      -- GIVEN: Prepare expectation
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ missingPerm)
      let expBody = encode expDto
      let expHeaders = resCtHeader : resCorsHeaders
      let expStatus = 403
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

assertCountInDB dbFunction appContext count = do
  eitherList <- runInContextIO dbFunction appContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  liftIO $ L.length list `shouldBe` count

getFirstFromDB dbFunction appContext = do
  eitherList <- runInContextIO dbFunction appContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  return . head $ list

getOneFromDB dbFunction appContext = do
  eitherOne <- runInContextIO dbFunction appContext
  liftIO $ isRight eitherOne `shouldBe` True
  let (Right one) = eitherOne
  return one
