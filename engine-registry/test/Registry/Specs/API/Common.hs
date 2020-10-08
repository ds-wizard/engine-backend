module Registry.Specs.API.Common where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.CaseInsensitive as CI
import Data.Either (isRight)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Registry.Bootstrap.Web
import Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Shared.Constant.Api
import Shared.Localization.Messages.Public

import Registry.Specs.Common
import SharedTest.Specs.API.Common
import SharedTest.Specs.Common

startWebApp :: AppContext -> IO Application
startWebApp appContext = do
  let baseContext =
        BaseContext
          { _baseContextServerConfig = appContext ^. serverConfig
          , _baseContextLocalization = appContext ^. localization
          , _baseContextBuildInfoConfig = appContext ^. buildInfoConfig
          , _baseContextPool = appContext ^. pool
          }
  let config = appContext ^. serverConfig
  let webPort = config ^. general . serverPort
  let env = config ^. general . environment
  return $ runMiddleware env $ runApp baseContext

reqAdminAuthHeader :: Header
reqAdminAuthHeader = ("Authorization", "Bearer GlobalToken")

reqUserAuthHeader :: Header
reqUserAuthHeader = ("Authorization", "Bearer NetherlandsToken")

reqStatisticsHeader :: [Header]
reqStatisticsHeader =
  [ (CI.mk . BS.pack $ xUserCountHeaderName, BS.pack . show $ iStat ^. userCount)
  , (CI.mk . BS.pack $ xPkgCountHeaderName, BS.pack . show $ iStat ^. pkgCount)
  , (CI.mk . BS.pack $ xQtnCountHeaderName, BS.pack . show $ iStat ^. qtnCount)
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
    let expDto = createUserError _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
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
    let expDto = createForbiddenError (_ERROR_VALIDATION__FORBIDDEN forbiddenReason)
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
