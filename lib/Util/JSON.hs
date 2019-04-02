module Util.JSON
  ( convertValueToOject
  , getField
  , getArrayField
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Localization
import Model.Error.ErrorHelpers

convertValueToOject value callback =
  case value of
    (Object obj) -> callback obj
    _ -> Left . createErrorWithErrorMessage $ _ERROR_UTIL_JSON__VALUE_IS_NOT_OBJECT

getField fieldName object callback =
  case HashMap.lookup (T.pack fieldName) object of
    Just field ->
      case eitherDecode . encode $ field of
        Right value -> callback value
        Left error -> Left . createErrorWithErrorMessage $ _ERROR_UTIL_JSON__CANT_DESERIALIZE_FIELD fieldName error
    Nothing -> Left . createErrorWithErrorMessage $ _ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT fieldName

getArrayField fieldName object callback = getField fieldName object parseArray
  where
    parseArray (Array field) = callback field
    parseArray _ = Left . createErrorWithErrorMessage $ _ERROR_UTIL_JSON__BAD_FIELD_TYPE fieldName "Array"
