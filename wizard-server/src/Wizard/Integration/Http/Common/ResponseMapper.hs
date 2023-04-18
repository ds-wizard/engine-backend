module Wizard.Integration.Http.Common.ResponseMapper (
  extractResponseHeader,
  extractResponseBody,
  extractNestedField,
  extractNestedStringField,
  extractStringField,
  extractIntField,
  convertToArray,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.Scientific (floatingOrInteger, toBoundedInteger)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Network.HTTP.Client (Response (..), responseHeaders)

import Shared.Common.Model.Error.Error
import Wizard.Localization.Messages.Internal

extractResponseHeader :: String -> Response BSL.ByteString -> Maybe String
extractResponseHeader headerName response =
  case L.find (\(h, _) -> h == (CI.mk . BS.pack $ headerName)) . responseHeaders $ response of
    Just (_, headerValue) -> Just . BS.unpack $ headerValue
    Nothing -> Nothing

extractResponseBody :: FromJSON a => Response BSL.ByteString -> Either AppError a
extractResponseBody response =
  case eitherDecode . responseBody $ response of
    Right body -> Right body
    Left error -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_DESERIALIZE_RESPONSE_BODY error

extractNestedField :: [String] -> Value -> Either AppError Value
extractNestedField [] response = Right response
extractNestedField (k : ks) (Object obj) =
  case KM.lookup (fromString k) obj of
    Just field -> extractNestedField ks field
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS (k : ks)
extractNestedField keys _ = Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS keys

extractNestedStringField :: [String] -> Value -> Either AppError String
extractNestedStringField [k] response = extractStringField k response
extractNestedStringField (k : ks) (Object obj) =
  case KM.lookup (fromString k) obj of
    Just field -> extractNestedStringField ks field
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS (k : ks)
extractNestedStringField keys _ = Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS keys

extractStringField :: String -> Value -> Either AppError String
extractStringField fieldName (Object obj) =
  case KM.lookup (fromString fieldName) obj of
    Just (String val) -> Right . T.unpack $ val
    Just (Number val) ->
      case floatingOrInteger val of
        Right float -> Right . show $ float
        Left int -> Right . show $ int
    Just (Bool val) -> Right . show $ val
    Just Null -> Right "null"
    _ -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD fieldName
extractStringField fieldName _ = Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD fieldName

extractIntField :: String -> Value -> Either AppError Int
extractIntField fieldName (Object obj) =
  case KM.lookup (fromString fieldName) obj of
    Just (Number val) ->
      case toBoundedInteger val of
        Just val -> Right val
        Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS fieldName
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS fieldName
extractIntField fieldName _ = Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_INTEGER_FIELD fieldName

convertToArray :: Value -> Either AppError [Value]
convertToArray (Array array) = Right . Vector.toList $ array
convertToArray _ = Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_FIELD_IS_NOT_ARRAY
