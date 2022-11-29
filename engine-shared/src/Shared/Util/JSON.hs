module Shared.Util.JSON where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

encodeJsonToString :: ToJSON dto => dto -> String
encodeJsonToString = T.unpack . TE.decodeUtf8 . BSL.toStrict . encode

convertValueToOject value callback =
  case value of
    (Object obj) -> callback obj
    _ -> Left . UserError $ _ERROR_UTIL_JSON__VALUE_IS_NOT_OBJECT

getField fieldName object callback =
  case KM.lookup (fromString fieldName) object of
    Just field ->
      case eitherDecode . encode $ field of
        Right value -> callback value
        Left error -> Left . UserError $ _ERROR_UTIL_JSON__CANT_DESERIALIZE_FIELD fieldName error
    Nothing -> Left . UserError $ _ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT fieldName

getArrayField fieldName object callback = getField fieldName object parseArray
  where
    parseArray (Array field) = callback field
    parseArray _ = Left . UserError $ _ERROR_UTIL_JSON__BAD_FIELD_TYPE fieldName "Array"

obj = Object . KM.fromList

arr = Array . V.fromList

str = String . T.pack

(.->) :: FromJSON a => Parser Object -> String -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: fromString key
