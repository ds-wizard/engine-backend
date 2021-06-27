module Wizard.Metamodel.Event.Version3.Common where

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

-- EventPathDTO
data EventPathItemDTO =
  EventPathItemDTO
    { _eventPathItemDTOPType :: String
    , _eventPathItemDTOUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

type EventPathDTO = [EventPathItemDTO]

-- EventPathJM
instance FromJSON EventPathItemDTO where
  parseJSON = simpleParseJSON "_eventPathItemDTO"

instance ToJSON EventPathItemDTO where
  toJSON = simpleToJSON "_eventPathItemDTO"

-- KnowledgeModelDTO
data MetricMeasureDTO =
  MetricMeasureDTO
    { _metricMeasureDTOMetricUuid :: U.UUID
    , _metricMeasureDTOMeasure :: Double
    , _metricMeasureDTOWeight :: Double
    }
  deriving (Show, Eq)

instance ToJSON MetricMeasureDTO where
  toJSON MetricMeasureDTO {..} =
    object
      [ "metricUuid" .= _metricMeasureDTOMetricUuid
      , "measure" .= _metricMeasureDTOMeasure
      , "weight" .= _metricMeasureDTOWeight
      ]

instance FromJSON MetricMeasureDTO where
  parseJSON (Object o) = do
    _metricMeasureDTOMetricUuid <- o .: "metricUuid"
    _metricMeasureDTOMeasure <- o .: "measure"
    _metricMeasureDTOWeight <- o .: "weight"
    return MetricMeasureDTO {..}
  parseJSON _ = mzero

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
    _ -> Left "Value is not object"

getField fieldName object callback =
  case HashMap.lookup (T.pack fieldName) object of
    Just field ->
      case eitherDecode . encode $ field of
        Right value -> callback value
        Left error -> Left $ "Cannot deserialize object: " <> fieldName <> "(" <> error <> ")"
    Nothing -> Left $ "Missing field in object: " <> fieldName

getArrayField fieldName object callback = getField fieldName object parseArray
  where
    parseArray (Array field) = callback field
    parseArray _ = Left $ "Bad field type: " <> fieldName <> "[Array]"

jsonSpecialFields :: String -> String
jsonSpecialFields "aType" = "type"
jsonSpecialFields "pType" = "type"
jsonSpecialFields "bundleId" = "id"
jsonSpecialFields "iId" = "id"
jsonSpecialFields "pId" = "id"
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

-- Util.String
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
