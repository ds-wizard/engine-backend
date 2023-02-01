module SharedTest.Specs.API.Common where

import Control.Exception (AssertionFailed (..), throw)
import Control.Monad.IO.Class
import Data.Aeson (Array, FromJSON, Object, ToJSON, Value (..), eitherDecode, encode)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable
import Data.String (fromString)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai.Test hiding (request)
import Test.Hspec
import qualified Test.Hspec.Expectations.Json as HJSON
import qualified Test.Hspec.Expectations.Pretty as HP
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api
import Shared.Constant.App
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.List (elems)
import Shared.Util.String (f')

reqCtHeader :: Header
reqCtHeader = contentTypeHeaderJSON

resCtHeaderPlain :: Header
resCtHeaderPlain = reqCtHeader

resCtHeader = "Content-Type" <:> "application/json"

resCtHeaderUtf8 = "Content-Type" <:> "application/json;charset=utf-8"

resCorsHeadersPlain :: [Header]
resCorsHeadersPlain =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Credential", "true")
  , ("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization")
  , ("Access-Control-Allow-Methods", "OPTIONS, HEAD, GET, POST, PUT, DELETE")
  ]

resCorsHeaders =
  [ "Access-Control-Allow-Origin" <:> "*"
  , "Access-Control-Allow-Credential" <:> "true"
  , "Access-Control-Allow-Headers" <:> "Origin, X-Requested-With, Content-Type, Accept, Authorization"
  , "Access-Control-Allow-Methods" <:> "OPTIONS, HEAD, GET, POST, PUT, DELETE"
  ]

shouldRespondWith r matcher = forM_ (match r matcher) (liftIO . expectationFailure)

-- ------------------------------------------------------------------------
-- TEST
-- ------------------------------------------------------------------------
createAuthTest reqMethod reqUrl reqHeaders reqBody =
  it "HTTP 401 UNAUTHORIZED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 401
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UnauthorizedError _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

createNotFoundTest reqMethod reqUrl reqHeaders reqBody entityName parameters =
  it "HTTP 404 NOT FOUND - entity doesn't exist" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName parameters)
      let expBody = encode expDto
      -- WHEN: Call APIA
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

createNotFoundTest' reqMethod reqUrl reqHeaders reqBody entityName parameters =
  createNotFoundTest
    reqMethod
    reqUrl
    reqHeaders
    reqBody
    entityName
    (("app_uuid", U.toString defaultAppUuid) : parameters)

-- ------------------------------------------------------------------------
-- ASSERT
-- ------------------------------------------------------------------------
assertResStatus resStatus expStatus = liftIO $ resStatus `shouldBe` expStatus

assertResHeaders resHeaders expHeaders = liftIO $ (expHeaders `elems` resHeaders) `shouldBe` True

destructResponse :: FromJSON resDto => SResponse -> (Int, ResponseHeaders, resDto)
destructResponse response =
  let (SResponse (Status status _) headers body) = response
      (Right resBody) = eitherDecode body
   in (status, headers, resBody)

destructResponse' :: (FromJSON resDto, MonadIO m) => SResponse -> m (Int, ResponseHeaders, resDto)
destructResponse' response = do
  let (SResponse (Status status _) headers body) = response
  case eitherDecode body of
    Right resBody -> return (status, headers, resBody)
    Left error -> do
      liftIO . print $ error
      throw . AssertionFailed $ f' "Failed to deserialize the response. Error: %s" [error]

assertResponse
  :: (ToJSON expDto, FromJSON expType)
  => Int
  -> ResponseHeaders
  -> expDto
  -> (expType -> expType)
  -> SResponse
  -> WaiSession () ()
assertResponse expStatus expHeaders expDto expType response =
  assertResponseWithoutFields expStatus expHeaders expDto expType response []

assertResponseWithoutFields
  :: (ToJSON expDto, FromJSON expType)
  => Int
  -> ResponseHeaders
  -> expDto
  -> (expType -> expType)
  -> SResponse
  -> [String]
  -> WaiSession () ()
assertResponseWithoutFields expStatus expHeaders expDto expType response fields =
  -- Prepare functions
  do
    let fieldModifier = foldl (\acc f -> KM.delete (fromString f) acc)
    -- Prepare expectation
    let (Right expBody) = eitherDecode . encode $ expDto :: Either String Object
    let expEntity = fieldModifier expBody fields
    let expResponse = MatchResponse expStatus expHeaders (encode expEntity)
    -- Prepare response
    let (SResponse (Status status _) headers bodyS) = response
    let (Right resBody) = eitherDecode bodyS :: Either String Object
    let resHeaders = filter (`elem` expHeaders) headers
    let resEntity = fieldModifier resBody fields
    let resResponse = MatchResponse status resHeaders (encode resEntity)
    -- Compare response with expectation
    case (status == expStatus, resHeaders == expHeaders) of
      (True, True) -> liftIO $ Object resEntity `HJSON.shouldBeJson` Object expEntity
      _ -> liftIO $ resResponse `HP.shouldBe` expResponse
    -- Check if response can be decoded to desired DTO
    let eDecodedDto = eitherDecode bodyS
    case eDecodedDto of
      Right decodedDto -> do
        let _ = expType decodedDto
        return ()
      Left error ->
        liftIO . expectationFailure $ f' "Response matched successfully. However, the result DTO couldn't be decoded. Error: %s" [error]

assertListResponse
  :: (ToJSON expDto, FromJSON expType)
  => Int
  -> ResponseHeaders
  -> expDto
  -> (expType -> expType)
  -> SResponse
  -> WaiSession () ()
assertListResponse expStatus expHeaders expDto expType response =
  -- Prepare expectation
  do
    let (Right expBody) = eitherDecode . encode $ expDto :: Either String Array
    let expResponse = MatchResponse expStatus expHeaders (encode expBody)
    -- Prepare response
    let (SResponse (Status status _) headers bodyS) = response
    let (Right resBody) = eitherDecode bodyS :: Either String Array
    let resHeaders = filter (`elem` expHeaders) headers
    let resResponse = MatchResponse status resHeaders (encode resBody)
    -- Compare response with expectation
    case (status == expStatus, resHeaders == expHeaders) of
      (True, True) -> liftIO $ Array resBody `HJSON.shouldBeJson` Array expBody
      _ -> liftIO $ resResponse `HP.shouldBe` expResponse
    -- Check if response can be decoded to desired DTO
    let eDecodedDto = eitherDecode bodyS
    case eDecodedDto of
      Right decodedDto -> do
        let _ = expType decodedDto
        return ()
      Left error ->
        liftIO . expectationFailure $ f' "Response matched successfully. However, the result DTO couldn't be decoded. Error: %s" [error]

assertEmptyResponse :: Int -> ResponseHeaders -> SResponse -> WaiSession () ()
assertEmptyResponse expStatus expHeaders response =
  -- Prepare expectation
  do
    let expResponse = MatchResponse expStatus expHeaders ""
    -- Prepare response
    let (SResponse (Status status _) headers bodyS) = response
    let resHeaders = filter (`elem` expHeaders) headers
    let resResponse = MatchResponse status resHeaders ""
    -- Compare response with expectation
    liftIO $ resResponse `HP.shouldBe` expResponse

data MatchResponse body = MatchResponse
  { status :: Int
  , headers :: ResponseHeaders
  , body :: body
  }
  deriving (Show, Eq)
