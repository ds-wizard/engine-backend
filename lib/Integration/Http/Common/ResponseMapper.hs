module Integration.Http.Common.ResponseMapper
  ( getResponseBody
  , deserializeResponseBody
  , extractResponseBody
  , extractNestedField
  , extractNestedStringField
  , extractStringField
  , convertToArray
  ) where

import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON, Value, eitherDecode)
import Data.Aeson.Lens (_Array, _String, _Value, key)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Network.Wreq (Response, responseBody)

import Localization
import Model.Error.Error

getResponseBody :: Response BSL.ByteString -> BSL.ByteString
getResponseBody response = response ^. responseBody

deserializeResponseBody :: FromJSON a => Response BSL.ByteString -> Either AppError a
deserializeResponseBody response =
  case eitherDecode $ response ^. responseBody of
    Right body -> Right body
    Left error -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_DESERIALIZE_RESPONSE_BODY error

extractResponseBody :: Response BSL.ByteString -> Either AppError Value
extractResponseBody response =
  case response ^? responseBody . _Value of
    Just body -> Right body
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_GET_RESPONSE_BODY

extractNestedField :: [String] -> Value -> Either AppError Value
extractNestedField [] response = Right response
extractNestedField (k:ks) response =
  case response ^? key (T.pack k) of
    Just field -> extractNestedField ks field
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS (k : ks)

extractNestedStringField :: [String] -> Value -> Either AppError String
extractNestedStringField (k:[]) response = extractStringField k response
extractNestedStringField (k:ks) response =
  case response ^? key (T.pack k) of
    Just field -> extractNestedStringField ks field
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS (k : ks)

extractStringField :: String -> Value -> Either AppError String
extractStringField fieldName record =
  case T.unpack <$> (record ^? key (T.pack fieldName) . _String) of
    Just val -> Right val
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD fieldName

convertToArray :: Value -> Either AppError [Value]
convertToArray response =
  case response ^? _Array of
    Just array -> Right . Vector.toList $ array
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_FIELD_IS_NOT_ARRAY
