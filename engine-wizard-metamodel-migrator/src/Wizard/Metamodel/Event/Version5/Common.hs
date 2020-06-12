module Wizard.Metamodel.Event.Version5.Common where

import Control.Monad
import Data.Aeson
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import GHC.Generics


-- EventFieldDTO
data EventFieldDTO a
  = NothingChangedDTO
  | ChangedValueDTO a
  deriving (Show, Eq)

instance Functor EventFieldDTO where
  fmap f (ChangedValueDTO a) = ChangedValueDTO (f a)
  fmap _ NothingChangedDTO = NothingChangedDTO

-- EventFieldJM
instance FromJSON a => FromJSON (EventFieldDTO a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValueDTO efValue
      else return NothingChangedDTO
  parseJSON _ = mzero

instance ToJSON a => ToJSON (EventFieldDTO a) where
  toJSON (ChangedValueDTO efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChangedDTO = object ["changed" .= False]

-- KnowledgeModelDTO/JM
data MetricMeasureDTO =
  MetricMeasureDTO
    { _metricMeasureDTOMetricUuid :: U.UUID
    , _metricMeasureDTOMeasure :: Double
    , _metricMeasureDTOWeight :: Double
    }
  deriving (Show, Eq, Generic)

instance ToJSON MetricMeasureDTO where
  toJSON = simpleToJSON "_metricMeasureDTO"

instance FromJSON MetricMeasureDTO where
  parseJSON = simpleParseJSON "_metricMeasureDTO"

-- KnowledgeModel
data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | TextQuestionValueType
  deriving (Show, Eq, Generic)

-- Api.Resource.KnowledgeModel.KnowledgeModelJM
instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

-- Util.JSON
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
jsonSpecialFields "iId" = "id"
jsonSpecialFields "pId" = "id"
jsonSpecialFields "intId" = "id"
jsonSpecialFields field = field

stripDTOSuffix :: String -> String
stripDTOSuffix field = fromMaybe field (stripSuffix "DTO" field)

simpleParseJSON fieldPrefix = genericParseJSON opts
  where
    opts = defaultOptions {fieldLabelModifier = jsonSpecialFields . lowerFirst . Prelude.drop (T.length fieldPrefix)}

simpleToJSON fieldPrefix = genericToJSON opts
  where
    opts = defaultOptions {fieldLabelModifier = jsonSpecialFields . lowerFirst . Prelude.drop (T.length fieldPrefix)}

simpleToJSON' typeFieldName fieldPrefix = genericToJSON opts
  where
    opts =
      defaultOptions
        { fieldLabelModifier = jsonSpecialFields . lowerFirst . Prelude.drop (T.length fieldPrefix)
        , tagSingleConstructors = True
        , sumEncoding = TaggedObject {tagFieldName = typeFieldName, contentsFieldName = "contents"}
        , constructorTagModifier = stripDTOSuffix
        }

-- Shared.Util.String
lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [toLower c]
lowerFirst (s:str) = toLower s : str

replace :: String -> String -> String -> String
replace name value string = T.unpack $ T.replace (T.pack name) (T.pack value) (T.pack string)

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix string = T.unpack <$> T.stripSuffix (T.pack suffix) (T.pack string)

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
