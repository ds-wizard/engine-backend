module SharedTest.Specs.API.Common where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, Object, ToJSON, Value, eitherDecode, encode)
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai.Test hiding (request)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as HP
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.List (elems)

reqCtHeader :: Header
reqCtHeader = contentTypeHeaderJSON

resCtHeaderPlain :: Header
resCtHeaderPlain = contentTypeHeaderJSON

resCtHeader = "Content-Type" <:> "application/json"

resCtHeaderUtf8 = "Content-Type" <:> "application/json;charset=utf-8"

resCtHeaderJavascript = "Content-Type" <:> "application/javascript"

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

createNotFoundTest reqMethod reqUrl reqHeaders reqBody entityName identificator =
  it "HTTP 404 NOT FOUND - entity doesn't exist" $
      -- GIVEN: Prepare expectation
   do
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator)
    let expBody = encode expDto
      -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ------------------------------------------------------------------------
-- ASSERT
-- ------------------------------------------------------------------------
assertResStatus resStatus expStatus = liftIO $ resStatus `shouldBe` expStatus

assertResHeaders resHeaders expHeaders = liftIO $ (expHeaders `elems` resHeaders) `shouldBe` True

destructResponse response =
  let (SResponse (Status status _) headers body) = response
      (Right resBody) = eitherDecode body
   in (status, headers, resBody)

assertResponse ::
     (ToJSON expDto, FromJSON expType)
  => Int
  -> ResponseHeaders
  -> expDto
  -> (expType -> expType)
  -> SResponse
  -> [String]
  -> WaiSession () ()
assertResponse = createAssertResponse fieldModifier fieldModifier
  where
    fieldModifier body =
      fmap (\(k, v) -> (T.unpack k, v)) . HashMap.toList . foldl (\acc f -> HashMap.delete (T.pack f) acc) body

assertResponse' ::
     (ToJSON expDto, FromJSON expType)
  => Int
  -> ResponseHeaders
  -> expDto
  -> (expType -> expType)
  -> SResponse
  -> [String]
  -> WaiSession () ()
assertResponse' = createAssertResponse expFieldModifier resFieldModifier
  where
    expFieldModifier expBody = fmap (\f -> (f, fromMaybe "EXPECTED_FIELD_MISSING" $ HashMap.lookup (T.pack f) expBody))
    resFieldModifier resBody = fmap (\f -> (f, fromMaybe "RESULT_FIELD_MISSING" $ HashMap.lookup (T.pack f) resBody))

createAssertResponse ::
     (ToJSON expDto, FromJSON expType)
  => (Object -> [String] -> [(String, Value)])
  -> (Object -> [String] -> [(String, Value)])
  -> Int
  -> ResponseHeaders
  -> expDto
  -> (expType -> expType)
  -> SResponse
  -> [String]
  -> WaiSession () ()
createAssertResponse expFieldModifier resFiledModifier expStatus expHeaders expDto expType response fields
  -- Prepare expectation
 = do
  let (Right expBody) = eitherDecode . encode $ expDto :: Either String Object
  let expEntity = expFieldModifier expBody fields
  let expResponse = MatchResponse expStatus expHeaders expEntity
  -- Prepare response
  let (SResponse (Status status _) headers bodyS) = response
  let (Right resBody) = eitherDecode bodyS :: Either String Object
  let resHeaders = filter (`elem` expHeaders) headers
  let resEntity = resFiledModifier resBody fields
  let resResponse = MatchResponse status resHeaders resEntity
  -- Compare response with expectation
  liftIO $ expResponse `HP.shouldBe` resResponse
  -- Check if response can be decoded to desired DTO
  let eDecodedDto = eitherDecode bodyS
  case eDecodedDto of
    Right decodedDto -> do
      let _ = expType decodedDto
      return ()
    Left _ -> liftIO $ expectationFailure "Response matched successfully. However, the result DTO couldn't be decoded."

data MatchResponse body =
  MatchResponse
    { status :: Int
    , headers :: ResponseHeaders
    , body :: body
    }
  deriving (Show, Eq)
