module Util.JSON
  ( convertValueToOject
  , getField
  , getArrayField
  , simpleParseJSON
  , simpleToJSON
  , simpleToJSON'
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Localization
import Model.Error.ErrorHelpers
import Util.String (lowerFirst, stripSuffix)

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

jsonSpecialFields :: String -> String
jsonSpecialFields "aType" = "type"
jsonSpecialFields "pType" = "type"
jsonSpecialFields "bundleId" = "id"
jsonSpecialFields "iId" = "id"
jsonSpecialFields "pId" = "id"
jsonSpecialFields "intId" = "id"
jsonSpecialFields field = field

stripDTOSuffix :: String -> String
stripDTOSuffix field = fromMaybe field (stripSuffix "DTO" field)

simpleParseJSON fieldPrefix = genericParseJSON opts
  where
    opts = defaultOptions {fieldLabelModifier = jsonSpecialFields . lowerFirst . drop (T.length fieldPrefix)}

simpleToJSON fieldPrefix = genericToJSON opts
  where
    opts = defaultOptions {fieldLabelModifier = jsonSpecialFields . lowerFirst . drop (T.length fieldPrefix)}

simpleToJSON' typeFieldName fieldPrefix = genericToJSON opts
  where
    opts =
      defaultOptions
      { fieldLabelModifier = jsonSpecialFields . lowerFirst . drop (T.length fieldPrefix)
      , tagSingleConstructors = True
      , sumEncoding = TaggedObject {tagFieldName = typeFieldName, contentsFieldName = "contents"}
      , constructorTagModifier = stripDTOSuffix
      }
