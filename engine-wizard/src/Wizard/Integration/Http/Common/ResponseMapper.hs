module Wizard.Integration.Http.Common.ResponseMapper (
  getResponseBody,
  deserializeResponseBody,
  extractResponseHeader,
  extractResponseBody,
  extractResponseBodyRaw,
  extractNestedField,
  extractNestedStringField,
  extractStringField,
  extractIntField,
  convertToArray,
) where

import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON, Value, eitherDecode)
import Data.Aeson.Lens (Primitive (..), key, _Array, _Primitive, _String, _Value)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Network.Wreq (Response, responseBody, responseHeader)

import Shared.Model.Error.Error
import Text.Read (readMaybe)
import Wizard.Localization.Messages.Internal

getResponseBody :: Response BSL.ByteString -> BSL.ByteString
getResponseBody response = response ^. responseBody

deserializeResponseBody :: FromJSON a => Response BSL.ByteString -> Either AppError a
deserializeResponseBody response =
  case eitherDecode $ response ^. responseBody of
    Right body -> Right body
    Left error -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_DESERIALIZE_RESPONSE_BODY error

extractResponseHeader :: String -> Response BSL.ByteString -> Maybe String
extractResponseHeader headerName response =
  case response ^? responseHeader (CI.mk . BS.pack $ headerName) of
    Just header -> Just . BS.unpack $ header
    Nothing -> Nothing

extractResponseBody :: Response BSL.ByteString -> Either AppError Value
extractResponseBody response =
  case response ^? responseBody . _Value of
    Just body -> Right body
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_GET_RESPONSE_BODY

extractResponseBodyRaw :: Response BSL.ByteString -> Either AppError BSL.ByteString
extractResponseBodyRaw response =
  case response ^? responseBody of
    Just body -> Right body
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_GET_RESPONSE_BODY

extractNestedField :: [String] -> Value -> Either AppError Value
extractNestedField [] response = Right response
extractNestedField (k : ks) response =
  case response ^? key (T.pack k) of
    Just field -> extractNestedField ks field
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS (k : ks)

extractNestedStringField :: [String] -> Value -> Either AppError String
extractNestedStringField [k] response = extractStringField k response
extractNestedStringField (k : ks) response =
  case response ^? key (T.pack k) of
    Just field -> extractNestedStringField ks field
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS (k : ks)

extractStringField :: String -> Value -> Either AppError String
extractStringField fieldName record =
  case record ^? key (T.pack fieldName) . _Primitive of
    Just (StringPrim val) -> Right . T.unpack $ val
    Just (NumberPrim val) ->
      case floatingOrInteger val of
        Right float -> Right . show $ float
        Left int -> Right . show $ int
    Just (BoolPrim val) -> Right . show $ val
    Just NullPrim -> Right "null"
    _ -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD fieldName

extractIntField :: String -> Value -> Either AppError Int
extractIntField fieldName record =
  case readMaybe <$> (T.unpack <$> (record ^? key (T.pack fieldName) . _String)) of
    Just (Just val) -> Right val
    _ -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_INTEGER_FIELD fieldName

convertToArray :: Value -> Either AppError [Value]
convertToArray response =
  case response ^? _Array of
    Just array -> Right . Vector.toList $ array
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_FIELD_IS_NOT_ARRAY
