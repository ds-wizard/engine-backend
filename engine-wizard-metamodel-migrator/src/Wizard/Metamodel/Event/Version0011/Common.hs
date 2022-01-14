module Wizard.Metamodel.Event.Version0011.Common where

import Control.Monad
import Data.Aeson
import qualified Data.Char as CH
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Data.Typeable
import qualified Data.UUID as U
import qualified Data.Vector as V
import GHC.Generics

-- Shared.Model.Event.EventField
data EventField a
  = NothingChanged
  | ChangedValue a
  deriving (Show, Eq, Generic, Typeable)

instance Functor EventField where
  fmap f (ChangedValue a) = ChangedValue (f a)
  fmap _ NothingChanged = NothingChanged

-- Shared.Api.Resource.Event.EventFieldJM
instance FromJSON a => FromJSON (EventField a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValue efValue
      else return NothingChanged
  parseJSON _ = mzero

instance ToJSON a => ToJSON (EventField a) where
  toJSON (ChangedValue efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChanged = object ["changed" .= False]

-- Shared.Model.KnowledgeModel.KnowledgeModel
data MetricMeasure =
  MetricMeasure
    { _metricMeasureMetricUuid :: U.UUID
    , _metricMeasureMeasure :: Double
    , _metricMeasureWeight :: Double
    }
  deriving (Show, Eq, Generic)

data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | TextQuestionValueType
  deriving (Show, Eq, Generic, Read)

-- Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM
instance ToJSON MetricMeasure where
  toJSON = simpleToJSON "_metricMeasure"

instance FromJSON MetricMeasure where
  parseJSON = simpleParseJSON "_metricMeasure"

instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

-- Shared.Util.JSON
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

obj = Object . HM.fromList

arr = Array . V.fromList

str = String . T.pack

-- Shared.Util.String
trim :: String -> String
trim = T.unpack . T.strip . T.pack

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [CH.toLower c]
lowerFirst (s:str) = CH.toLower s : str

toLower :: String -> String
toLower = fmap CH.toLower

replace :: String -> String -> String -> String
replace name value string = T.unpack $ T.replace (T.pack name) (T.pack value) (T.pack string)

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs

takeLastOf :: String -> String -> Maybe String
takeLastOf separator string =
  case splitOn separator string of
    [] -> Nothing
    xs -> Just . last $ xs

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix string = T.unpack <$> T.stripSuffix (T.pack suffix) (T.pack string)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix name = T.isSuffixOf (T.pack suffix) (T.pack name)

f' :: String -> [String] -> String
f' str terms =
  case str of
    '%':'s':rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ f' rest (drop 1 terms)
    '%':'%':'s':rest -> '%' : 's' : f' rest terms
    a:rest -> a : f' rest terms
    [] -> []

printTuples :: [(String, String)] -> String
printTuples = L.intercalate ", " . fmap printTuple
  where
    printTuple (key, value) = key ++ ": " ++ value

-- Shared (Errors and Localization)
type LocaleKey = String

type DefaultValue = String

type Variable = String

data LocaleRecord =
  LocaleRecord LocaleKey DefaultValue [Variable]
  deriving (Show, Eq)

type FormError = LocaleRecord

type FieldError = (String, LocaleRecord)

data AppError
  = ValidationError [FormError] [FieldError]
  | UserError LocaleRecord
  | UnauthorizedError LocaleRecord
  | ForbiddenError LocaleRecord
  | NotExistsError LocaleRecord
  | GeneralServerError String
  deriving (Show, Eq)

_ERROR_UTIL_JSON__VALUE_IS_NOT_OBJECT =
  LocaleRecord
    "error.util.json.value_is_not_object"
    "Problem in deserialization of JSON (reason: provided value is not an object)"
    []

_ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT fieldName =
  LocaleRecord
    "error.util.json.missing_field_in_object"
    "Problem in deserialization of JSON (reason: missing '%s' key in provided object)"
    [fieldName]

_ERROR_UTIL_JSON__BAD_FIELD_TYPE fieldName fieldType =
  LocaleRecord
    "error.util.json.bad_field_type"
    "Problem in deserialization of JSON (reason: type of '%s' in provided object is not '%s')"
    [fieldName, fieldType]

_ERROR_UTIL_JSON__CANT_DESERIALIZE_FIELD fieldName error =
  LocaleRecord
    "error.util.json.cant_deserialize_field"
    "Problem in deserialization of JSON (field: '%s', error: %s)"
    [fieldName, error]

-- Shared.Util.Reflection
constructorName :: (HasConstructor (Rep a), Generic a) => a -> String
constructorName = genericConstructorName . from

class HasConstructor (f :: * -> *) where
  genericConstructorName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstructorName (M1 x) = genericConstructorName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstructorName (L1 l) = genericConstructorName l
  genericConstructorName (R1 r) = genericConstructorName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstructorName = conName

-- Shared.Model.Common.MapEntry
data MapEntry key value =
  MapEntry
    { _mapEntryKey :: key
    , _mapEntryValue :: value
    }
  deriving (Show, Eq, Generic)

mapEntryToMap :: Ord key => [MapEntry key value] -> M.Map key value
mapEntryToMap = M.fromList . fmap (\(MapEntry key value) -> (key, value))

-- Shared.Api.Resource.Common.MapEntryJM
instance (ToJSON key, ToJSON value) => ToJSON (MapEntry key value) where
  toJSON = simpleToJSON "_mapEntry"

instance (FromJSON key, FromJSON value) => FromJSON (MapEntry key value) where
  parseJSON = simpleParseJSON "_mapEntry"
