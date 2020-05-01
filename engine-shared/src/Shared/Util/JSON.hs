module Shared.Util.JSON where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Reflection (HasConstructor, constructorName)
import Shared.Util.String (lowerFirst, splitOn, stripSuffix)

convertValueToOject value callback =
  case value of
    (Object obj) -> callback obj
    _ -> Left . UserError $ _ERROR_UTIL_JSON__VALUE_IS_NOT_OBJECT

getField fieldName object callback =
  case HashMap.lookup (T.pack fieldName) object of
    Just field ->
      case eitherDecode . encode $ field of
        Right value -> callback value
        Left error -> Left . UserError $ _ERROR_UTIL_JSON__CANT_DESERIALIZE_FIELD fieldName error
    Nothing -> Left . UserError $ _ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT fieldName

getArrayField fieldName object callback = getField fieldName object parseArray
  where
    parseArray (Array field) = callback field
    parseArray _ = Left . UserError $ _ERROR_UTIL_JSON__BAD_FIELD_TYPE fieldName "Array"

jsonSpecialFields :: String -> String
jsonSpecialFields "aType" = "type"
jsonSpecialFields "pType" = "type"
jsonSpecialFields "bundleId" = "id"
jsonSpecialFields "intId" = "id"
jsonSpecialFields "aId" = "id"
jsonSpecialFields "dId" = "id"
jsonSpecialFields "iId" = "id"
jsonSpecialFields "pId" = "id"
jsonSpecialFields "sId" = "id"
jsonSpecialFields field = field

stripDTOSuffix :: String -> String
stripDTOSuffix field = fromMaybe field (stripSuffix "DTO" field)

stripDTOSuffix' :: String -> String
stripDTOSuffix' field = fromMaybe field (stripSuffix "DTO'" field)

simpleParseJSON fieldPrefix = genericParseJSON (createOptions fieldPrefix)

toSumJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
toSumJSON = genericToJSON (defaultOptions {sumEncoding = UntaggedValue})

toSumJSON' :: (Generic a, GToJSON Zero (Rep a), HasConstructor (Rep a)) => T.Text -> a -> Value
toSumJSON' typeFieldName dto =
  case genericToJSON (defaultOptions {sumEncoding = UntaggedValue}) dto of
    Object o ->
      Object $ HM.union o (HM.fromList [(typeFieldName, String . T.pack . stripDTOSuffix' . constructorName $ dto)])

simpleToJSON fieldPrefix = genericToJSON (createOptions fieldPrefix)

simpleToJSON' fieldPrefix typeFieldName = genericToJSON (createOptions' fieldPrefix typeFieldName)

simpleToJSON'' fieldPrefix additionalData dto =
  case simpleToJSON fieldPrefix dto of
    Object o -> Object $ HM.union o (HM.fromList additionalData)

createOptions :: String -> Options
createOptions fieldPrefix =
  defaultOptions {fieldLabelModifier = jsonSpecialFields . lowerFirst . drop (length fieldPrefix)}

createOptions' :: String -> String -> Options
createOptions' fieldPrefix typeFieldName =
  defaultOptions
    { fieldLabelModifier = jsonSpecialFields . lowerFirst . drop (length fieldPrefix)
    , tagSingleConstructors = True
    , sumEncoding = TaggedObject {tagFieldName = typeFieldName, contentsFieldName = "contents"}
    , constructorTagModifier = stripDTOSuffix
    }

simpleOptions :: Options
simpleOptions = defaultOptions {fieldLabelModifier = fieldLabelModifierFn}

fieldLabelModifierFn :: String -> String
fieldLabelModifierFn value = jsonSpecialFields . lowerFirst $ splitOn "DTO" value !! 1
