module Wizard.Integration.Http.TypeHint.ResponseMapper (
  toRetrieveLegacyTypeHintsResponse,
  toRetrieveTypeHintsResponse,
  toTypeHintTestResponse,
  toTypeHintTestResponseError,
  toResponseErrorMessage,
) where

import Control.Exception (SomeException, displayException, fromException)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import Prelude hiding (lookup)

import Shared.Common.Integration.Http.Common.ResponseMapper
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Ginger
import Shared.Common.Util.String (splitOn)
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Util.Jinja (renderJinjaBatch)

toRetrieveLegacyTypeHintsResponse :: ApiLegacyIntegration -> Response BSL.ByteString -> Either String [TypeHintLegacyIDTO]
toRetrieveLegacyTypeHintsResponse intConfig response =
  case extractResponseBody response >>= extractNestedField listField >>= convertToArray >>= mapRecords of
    Right dto -> Right dto
    Left error -> Left . show $ error
  where
    listField =
      case intConfig.responseListField of
        Just responseListField -> splitOn "." responseListField
        Nothing -> []
    mapRecords :: [Value] -> Either AppError [TypeHintLegacyIDTO]
    mapRecords = Right . rights . fmap mapRecord
    mapRecord :: Value -> Either String TypeHintLegacyIDTO
    mapRecord record = do
      let contextMap = HM.fromList [("item", record)]
      itemId <-
        case intConfig.responseItemId of
          Just responseItemId -> do
            result <- renderEither responseItemId contextMap
            Right . Just . T.unpack $ result
          Nothing -> Right Nothing
      itemTemplate <- renderEither intConfig.responseItemTemplate contextMap
      Right $ TypeHintLegacyIDTO {intId = itemId, name = T.unpack itemTemplate}

toRetrieveTypeHintsResponse :: ApiIntegration -> Response BSL.ByteString -> IO (Either String [TypeHintIDTO])
toRetrieveTypeHintsResponse intConfig response = do
  case extractResponseBody response >>= extractNestedField listField >>= convertToArray of
    Left err -> return . Left . show $ err
    Right records' -> do
      let recordsInItems = fmap (\v -> object ["item" .= v]) records'
      values <- renderJinjaBatch intConfig.responseItemTemplate recordsInItems
      valuesForSelect <- case intConfig.responseItemTemplateForSelection of
        Just templateForSelection -> (fmap . fmap . fmap $ Just) $ renderJinjaBatch templateForSelection recordsInItems
        Nothing -> return $ fmap (const (Right Nothing)) recordsInItems
      return . Right . rights . fmap mapRecord $ zip3 valuesForSelect values records'
  where
    listField =
      case intConfig.responseListField of
        Just responseListField -> splitOn "." responseListField
        Nothing -> []
    mapRecord :: (Either String (Maybe String), Either String String, Value) -> Either String TypeHintIDTO
    mapRecord (Left err, _, _) = Left err
    mapRecord (_, Left err, _) = Left err
    mapRecord (Right valueForSelection, Right value, record) =
      Right $ TypeHintIDTO {valueForSelection = valueForSelection, value = value, raw = record}

toTypeHintTestResponseError :: TypeHintRequest -> String -> TypeHintExchange
toTypeHintTestResponseError testRequest message =
  TypeHintExchange
    { request = testRequest
    , response =
        RequestFailedTypeHintResponse' $
          RequestFailedTypeHintResponse
            { message = message
            }
    }

toTypeHintTestResponse :: TypeHintRequest -> Response BSL.ByteString -> TypeHintExchange
toTypeHintTestResponse testRequest httpResponse
  | 200 <= status && status < 300 =
      TypeHintExchange
        { request = testRequest
        , response =
            SuccessTypeHintResponse' $
              SuccessTypeHintResponse
                { status = status
                , contentType = contentType
                , body = body
                }
        }
  | otherwise =
      TypeHintExchange
        { request = testRequest
        , response =
            RemoteErrorTypeHintResponse' $
              RemoteErrorTypeHintResponse
                { status = status
                , contentType = contentType
                , body = body
                }
        }
  where
    status = statusCode . responseStatus $ httpResponse
    contentType = fmap (T.unpack . T.decodeUtf8) $ lookup hContentType (responseHeaders httpResponse)
    body = TL.unpack . TL.decodeUtf8 $ responseBody httpResponse

toResponseErrorMessage :: SomeException -> String
toResponseErrorMessage e =
  case fromException e of
    Just (HttpExceptionRequest _ content) -> describeContent content
    Just (InvalidUrlException url reason) ->
      "Invalid URL: " ++ url ++ " (" ++ reason ++ ")."
    Nothing -> displayException e
  where
    describeContent :: HttpExceptionContent -> String
    describeContent (StatusCodeException resp _) =
      "HTTP error code returned: " ++ show (responseStatus resp) ++ "."
    describeContent (TooManyRedirects _) =
      "Too many redirects: the server responded with too many redirects for a request."
    describeContent OverlongHeaders =
      "Response headers too large: too many total bytes in the HTTP header were returned by the server."
    describeContent ResponseTimeout =
      "Response timed out: the server took too long to return a response."
    describeContent ConnectionTimeout =
      "Connection timed out: attempting to connect to the server timed out."
    describeContent (ConnectionFailure _) =
      "Connection failure: the service does not exist or is not reachable."
    describeContent (InvalidStatusLine _) =
      "Invalid status line: the status line returned by the server could not be parsed."
    describeContent (InvalidHeader bs) =
      "Invalid header: the given response header '" ++ BS.unpack bs ++ "' could not be parsed."
    describeContent (InvalidRequestHeader bs) =
      "Invalid request header: the given request header " ++ BS.unpack bs ++ " is not compliant."
    describeContent (InternalException _) =
      "Internal exception: an exception was raised by an underlying library when performing the request."
    describeContent (ProxyConnectException host port status) =
      "Proxy connect failed: " ++ BS.unpack host ++ ":" ++ show port ++ " - " ++ show status ++ "."
    describeContent NoResponseDataReceived =
      "No response data received."
    describeContent TlsNotSupported =
      "TLS is not supported by the HTTP manager."
    describeContent (WrongRequestBodyStreamSize expected actual) =
      "Request body size mismatch: expected " ++ show expected ++ ", got " ++ show actual ++ "."
    describeContent (ResponseBodyTooShort expected actual) =
      "Response body too short: expected " ++ show expected ++ ", got " ++ show actual ++ "."
    describeContent InvalidChunkHeaders =
      "Invalid chunk headers: a chunked response body had invalid headers."
    describeContent IncompleteHeaders =
      "Incomplete headers received: an incomplete set of response headers were returned."
    describeContent (InvalidDestinationHost bs) =
      "Invalid destination host: " ++ BS.unpack bs
    describeContent (HttpZlibException _) =
      "Zlib exception: an exception was thrown when inflating a response body."
    describeContent (InvalidProxyEnvironmentVariable name val) =
      "Invalid proxy environment variable " ++ T.unpack name ++ " = " ++ T.unpack val ++ "."
    describeContent ConnectionClosed =
      "Connection already closed."
    describeContent (InvalidProxySettings txt) =
      "Invalid proxy settings: " ++ T.unpack txt ++ "."
