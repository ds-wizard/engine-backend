module Registry.Specs.API.Common where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.CaseInsensitive as CI
import Data.Either (isRight)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Servant (serve)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Middleware.LoggingMiddleware
import Registry.Api.Web
import Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Statistics.InstanceStatistics
import Shared.Common.Bootstrap.Web
import Shared.Common.Constant.Api
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error

import Registry.Specs.Common
import SharedTest.Specs.API.Common

startWebApp :: BaseContext -> AppContext -> IO Application
startWebApp baseContext appContext = do
  let config = appContext.serverConfig
  let webPort = config.general.serverPort
  let env = config.general.environment
  return $ runMiddleware env loggingMiddleware $ serve webApi (webServer baseContext)

reqAdminAuthHeader :: Header
reqAdminAuthHeader = ("Authorization", "Bearer GlobalToken")

reqUserAuthHeader :: Header
reqUserAuthHeader = ("Authorization", "Bearer NetherlandsToken")

reqStatisticsHeader :: [Header]
reqStatisticsHeader =
  [ (CI.mk . BS.pack $ xUserCountHeaderName, BS.pack . show $ iStat.userCount)
  , (CI.mk . BS.pack $ xKnowledgeModelPackageCountHeaderName, BS.pack . show $ iStat.pkgCount)
  , (CI.mk . BS.pack $ xQtnCountHeaderName, BS.pack . show $ iStat.qtnCount)
  , (CI.mk . BS.pack $ xKnowledgeModelEditorCountHeaderName, BS.pack . show $ iStat.kmEditorCount)
  , (CI.mk . BS.pack $ xDocCountHeaderName, BS.pack . show $ iStat.docCount)
  , (CI.mk . BS.pack $ xTmlCountHeaderName, BS.pack . show $ iStat.tmlCount)
  ]

-- ----------------------------------------------------
-- TESTS
-- ----------------------------------------------------
createInvalidJsonTest reqMethod reqUrl missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAdminAuthHeader, reqCtHeader]
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

createForbiddenTest reqMethod reqUrl reqHeaders reqBody forbiddenReason =
  it "HTTP 403 FORBIDDEN" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError (_ERROR_VALIDATION__FORBIDDEN forbiddenReason)
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ASSERT
-- ----------------------------------------------------
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
