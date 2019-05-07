module Integration.Http.Common.ResponseMapper
  ( extractResponseBody
  , extractNestedField
  , extractStringField
  , convertToArray
  ) where

import Control.Lens ((^?))
import Data.Aeson (Value)
import Data.Aeson.Lens (_Array, _String, _Value, key)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Network.Wreq (Response, responseBody)

import Localization
import Model.Error.Error

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
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS

extractStringField :: String -> Value -> Maybe String
extractStringField fieldName record = T.unpack <$> (record ^? key (T.pack fieldName) . _String)

convertToArray :: Value -> Either AppError [Value]
convertToArray response =
  case response ^? _Array of
    Just array -> Right . Vector.toList $ array
    Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_FIELD_IS_NOT_ARRAY
