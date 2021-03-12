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
jsonSpecialFields "gType" = "type"
jsonSpecialFields "pType" = "type"
jsonSpecialFields "bundleId" = "id"
jsonSpecialFields "intId" = "id"
jsonSpecialFields "aId" = "id"
jsonSpecialFields "dId" = "id"
jsonSpecialFields "gId" = "id"
jsonSpecialFields "iId" = "id"
jsonSpecialFields "pId" = "id"
jsonSpecialFields "sId" = "id"
jsonSpecialFields "tId" = "id"
jsonSpecialFields field = field

stripCustomSuffix :: String -> String -> String
stripCustomSuffix prefix f1 =
  let f2 = fromMaybe f1 (stripSuffix "'" f1)
   in fromMaybe f2 (stripSuffix prefix f2)

stripDTOSuffix :: String -> String
stripDTOSuffix = stripCustomSuffix "DTO"

simpleParseJSON fieldPrefix = genericParseJSON (createOptions fieldPrefix)

simpleParseJSON' fieldPrefix typeFieldName = genericToJSON (createOptions' fieldPrefix typeFieldName)

toSumJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
toSumJSON = genericToJSON (defaultOptions {sumEncoding = UntaggedValue})

toSumJSON' :: (Generic a, GToJSON Zero (Rep a), HasConstructor (Rep a)) => T.Text -> a -> Value
toSumJSON' typeFieldName dto =
  case genericToJSON (defaultOptions {sumEncoding = UntaggedValue}) dto of
    Object o ->
      Object $ HM.union o (HM.fromList [(typeFieldName, String . T.pack . stripDTOSuffix . constructorName $ dto)])

toSumJSON'' :: (Generic a, GToJSON Zero (Rep a), HasConstructor (Rep a)) => T.Text -> String -> a -> Value
toSumJSON'' typeFieldName suffix dto =
  case genericToJSON (defaultOptions {sumEncoding = UntaggedValue}) dto of
    Object o ->
      Object $
      HM.union o (HM.fromList [(typeFieldName, String . T.pack . stripCustomSuffix suffix . constructorName $ dto)])

simpleToJSON fieldPrefix = genericToJSON (createOptions fieldPrefix)

simpleToJSON' fieldPrefix typeFieldName = genericToJSON (createOptions' fieldPrefix typeFieldName)

simpleToJSON'' fieldPrefix additionalData dto =
  case simpleToJSON fieldPrefix dto of
    Object o -> Object $ HM.union o (HM.fromList additionalData)

simpleToJSON''' fieldPrefix typeFieldName suffix = genericToJSON (createOptions'' fieldPrefix typeFieldName suffix)

createOptions :: String -> Options
createOptions fieldPrefix = defaultOptions {fieldLabelModifier = fieldLabelModifierFnWithoutDTO fieldPrefix}

createOptions' :: String -> String -> Options
createOptions' fieldPrefix typeFieldName =
  defaultOptions
    { fieldLabelModifier = fieldLabelModifierFnWithoutDTO fieldPrefix
    , tagSingleConstructors = True
    , sumEncoding = TaggedObject {tagFieldName = typeFieldName, contentsFieldName = "contents"}
    , constructorTagModifier = stripDTOSuffix
    }

createOptions'' :: String -> String -> String -> Options
createOptions'' fieldPrefix typeFieldName suffix =
  defaultOptions
    { fieldLabelModifier = fieldLabelModifierFnWithoutDTO fieldPrefix
    , tagSingleConstructors = True
    , sumEncoding = TaggedObject {tagFieldName = typeFieldName, contentsFieldName = "contents"}
    , constructorTagModifier = stripCustomSuffix suffix
    }

simpleOptions :: Options
simpleOptions = defaultOptions {fieldLabelModifier = fieldLabelModifierFn}

simpleOptions''' :: Options
simpleOptions''' =
  defaultOptions
    { fieldLabelModifier = fieldLabelModifierFn
    , tagSingleConstructors = True
    , sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "contents"}
    , constructorTagModifier = stripDTOSuffix
    }

createSimpleOptions'''' :: String -> Options
createSimpleOptions'''' parentEntityName =
  defaultOptions
    { fieldLabelModifier = fieldLabelModifierFnWithParentEntityNameDTO parentEntityName
    , tagSingleConstructors = True
    , sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "contents"}
    , constructorTagModifier = stripDTOSuffix
    }

fieldLabelModifierFn :: String -> String
fieldLabelModifierFn value = jsonSpecialFields . lowerFirst $ splitOn "DTO" value !! 1

fieldLabelModifierFnWithoutDTO :: String -> String -> String
fieldLabelModifierFnWithoutDTO fieldPrefix = jsonSpecialFields . lowerFirst . drop (length fieldPrefix)

fieldLabelModifierFnWithParentEntityNameDTO :: String -> String -> String
fieldLabelModifierFnWithParentEntityNameDTO parentEntityName value =
  jsonSpecialFields . lowerFirst $ splitOn parentEntityName value !! 1
