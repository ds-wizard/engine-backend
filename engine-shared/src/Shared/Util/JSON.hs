module Shared.Util.JSON where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Reflection (HasConstructor, constructorName)
import Shared.Util.String (lowerFirst, splitOn, stripSuffix)

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
jsonSpecialFields "qiId" = "id"
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

toSumJSON' :: (Generic a, GToJSON Zero (Rep a), HasConstructor (Rep a)) => String -> a -> Value
toSumJSON' typeFieldName dto =
  case genericToJSON (defaultOptions {sumEncoding = UntaggedValue}) dto of
    Object o ->
      Object $
      KM.union o (KM.fromList [(fromString typeFieldName, String . T.pack . stripDTOSuffix . constructorName $ dto)])

toSumJSON'' :: (Generic a, GToJSON Zero (Rep a), HasConstructor (Rep a)) => String -> String -> a -> Value
toSumJSON'' typeFieldName suffix dto =
  case genericToJSON (defaultOptions {sumEncoding = UntaggedValue}) dto of
    Object o ->
      Object $
      KM.union
        o
        (KM.fromList [(fromString typeFieldName, String . T.pack . stripCustomSuffix suffix . constructorName $ dto)])

simpleToJSON fieldPrefix = genericToJSON (createOptions fieldPrefix)

simpleToJSON' fieldPrefix typeFieldName = genericToJSON (createOptions' fieldPrefix typeFieldName)

simpleToJSON'' fieldPrefix additionalData dto =
  case simpleToJSON fieldPrefix dto of
    Object o -> Object $ KM.union o (KM.fromList additionalData)

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

obj = Object . KM.fromList

arr = Array . V.fromList

str = String . T.pack

(.->) :: FromJSON a => Parser Object -> String -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: fromString key
