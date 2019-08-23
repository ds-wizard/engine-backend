module DSW.Metamodel.Event.Version3 where

import Control.Monad
import Data.Aeson
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HashMap
import Data.Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

-- Created from dsw-server @ba78aef208e899807fd1e46a5615df7a53904d82
-- * Api.Resource.Event.*
-- * Api.Resource.KnowledgeModel.KnowledgeModelDTO
-- * Model.KnowledgeModel.KnowledgeModel
-- * Util.JSON
-- * Util.String
-- EventDTO
data EventDTO
  = AddKnowledgeModelEventDTO' AddKnowledgeModelEventDTO
  | EditKnowledgeModelEventDTO' EditKnowledgeModelEventDTO
  | AddChapterEventDTO' AddChapterEventDTO
  | EditChapterEventDTO' EditChapterEventDTO
  | DeleteChapterEventDTO' DeleteChapterEventDTO
  | AddQuestionEventDTO' AddQuestionEventDTO
  | EditQuestionEventDTO' EditQuestionEventDTO
  | DeleteQuestionEventDTO' DeleteQuestionEventDTO
  | AddAnswerEventDTO' AddAnswerEventDTO
  | EditAnswerEventDTO' EditAnswerEventDTO
  | DeleteAnswerEventDTO' DeleteAnswerEventDTO
  | AddExpertEventDTO' AddExpertEventDTO
  | EditExpertEventDTO' EditExpertEventDTO
  | DeleteExpertEventDTO' DeleteExpertEventDTO
  | AddReferenceEventDTO' AddReferenceEventDTO
  | EditReferenceEventDTO' EditReferenceEventDTO
  | DeleteReferenceEventDTO' DeleteReferenceEventDTO
  | AddTagEventDTO' AddTagEventDTO
  | EditTagEventDTO' EditTagEventDTO
  | DeleteTagEventDTO' DeleteTagEventDTO
  | AddIntegrationEventDTO' AddIntegrationEventDTO
  | EditIntegrationEventDTO' EditIntegrationEventDTO
  | DeleteIntegrationEventDTO' DeleteIntegrationEventDTO
  deriving (Show, Eq)

-- EventJM
instance ToJSON EventDTO where
  toJSON (AddKnowledgeModelEventDTO' event) = toJSON event
  toJSON (EditKnowledgeModelEventDTO' event) = toJSON event
  toJSON (AddChapterEventDTO' event) = toJSON event
  toJSON (EditChapterEventDTO' event) = toJSON event
  toJSON (DeleteChapterEventDTO' event) = toJSON event
  toJSON (AddQuestionEventDTO' event) = toJSON event
  toJSON (EditQuestionEventDTO' event) = toJSON event
  toJSON (DeleteQuestionEventDTO' event) = toJSON event
  toJSON (AddAnswerEventDTO' event) = toJSON event
  toJSON (EditAnswerEventDTO' event) = toJSON event
  toJSON (DeleteAnswerEventDTO' event) = toJSON event
  toJSON (AddExpertEventDTO' event) = toJSON event
  toJSON (EditExpertEventDTO' event) = toJSON event
  toJSON (DeleteExpertEventDTO' event) = toJSON event
  toJSON (AddReferenceEventDTO' event) = toJSON event
  toJSON (EditReferenceEventDTO' event) = toJSON event
  toJSON (DeleteReferenceEventDTO' event) = toJSON event
  toJSON (AddTagEventDTO' event) = toJSON event
  toJSON (EditTagEventDTO' event) = toJSON event
  toJSON (DeleteTagEventDTO' event) = toJSON event
  toJSON (AddIntegrationEventDTO' event) = toJSON event
  toJSON (EditIntegrationEventDTO' event) = toJSON event
  toJSON (DeleteIntegrationEventDTO' event) = toJSON event

instance FromJSON EventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "eventType"
    case eventType of
      "AddKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEventDTO' event)
      "EditKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (EditKnowledgeModelEventDTO' event)
      "AddChapterEvent" -> parseJSON (Object o) >>= \event -> return (AddChapterEventDTO' event)
      "EditChapterEvent" -> parseJSON (Object o) >>= \event -> return (EditChapterEventDTO' event)
      "DeleteChapterEvent" -> parseJSON (Object o) >>= \event -> return (DeleteChapterEventDTO' event)
      "AddQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddQuestionEventDTO' event)
      "EditQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditQuestionEventDTO' event)
      "DeleteQuestionEvent" -> parseJSON (Object o) >>= \event -> return (DeleteQuestionEventDTO' event)
      "AddAnswerEvent" -> parseJSON (Object o) >>= \event -> return (AddAnswerEventDTO' event)
      "EditAnswerEvent" -> parseJSON (Object o) >>= \event -> return (EditAnswerEventDTO' event)
      "DeleteAnswerEvent" -> parseJSON (Object o) >>= \event -> return (DeleteAnswerEventDTO' event)
      "AddExpertEvent" -> parseJSON (Object o) >>= \event -> return (AddExpertEventDTO' event)
      "EditExpertEvent" -> parseJSON (Object o) >>= \event -> return (EditExpertEventDTO' event)
      "DeleteExpertEvent" -> parseJSON (Object o) >>= \event -> return (DeleteExpertEventDTO' event)
      "AddReferenceEvent" -> parseJSON (Object o) >>= \event -> return (AddReferenceEventDTO' event)
      "EditReferenceEvent" -> parseJSON (Object o) >>= \event -> return (EditReferenceEventDTO' event)
      "DeleteReferenceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteReferenceEventDTO' event)
      "AddTagEvent" -> parseJSON (Object o) >>= \event -> return (AddTagEventDTO' event)
      "EditTagEvent" -> parseJSON (Object o) >>= \event -> return (EditTagEventDTO' event)
      "DeleteTagEvent" -> parseJSON (Object o) >>= \event -> return (DeleteTagEventDTO' event)
      "AddIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (AddIntegrationEventDTO' event)
      "EditIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (EditIntegrationEventDTO' event)
      "DeleteIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (DeleteIntegrationEventDTO' event)
      _ -> fail "One of the events has unsupported eventType"
  parseJSON _ = mzero

-- AnswerEventDTO
data AddAnswerEventDTO = AddAnswerEventDTO
  { _addAnswerEventDTOUuid :: U.UUID
  , _addAnswerEventDTOPath :: EventPathDTO
  , _addAnswerEventDTOAnswerUuid :: U.UUID
  , _addAnswerEventDTOLabel :: String
  , _addAnswerEventDTOAdvice :: Maybe String
  , _addAnswerEventDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data EditAnswerEventDTO = EditAnswerEventDTO
  { _editAnswerEventDTOUuid :: U.UUID
  , _editAnswerEventDTOPath :: EventPathDTO
  , _editAnswerEventDTOAnswerUuid :: U.UUID
  , _editAnswerEventDTOLabel :: EventFieldDTO String
  , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
  , _editAnswerEventDTOFollowUpUuids :: EventFieldDTO [U.UUID]
  , _editAnswerEventDTOMetricMeasures :: EventFieldDTO [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO = DeleteAnswerEventDTO
  { _deleteAnswerEventDTOUuid :: U.UUID
  , _deleteAnswerEventDTOPath :: EventPathDTO
  , _deleteAnswerEventDTOAnswerUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- AnsvertEventJM
instance FromJSON AddAnswerEventDTO where
  parseJSON = simpleParseJSON "_addAnswerEventDTO"

instance ToJSON AddAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_addAnswerEventDTO"

instance FromJSON EditAnswerEventDTO where
  parseJSON = simpleParseJSON "_editAnswerEventDTO"

instance ToJSON EditAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_editAnswerEventDTO"

instance FromJSON DeleteAnswerEventDTO where
  parseJSON = simpleParseJSON "_deleteAnswerEventDTO"

instance ToJSON DeleteAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteAnswerEventDTO"

-- ChapterEventDTO
data AddChapterEventDTO = AddChapterEventDTO
  { _addChapterEventDTOUuid :: U.UUID
  , _addChapterEventDTOPath :: EventPathDTO
  , _addChapterEventDTOChapterUuid :: U.UUID
  , _addChapterEventDTOTitle :: String
  , _addChapterEventDTOText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEventDTO = EditChapterEventDTO
  { _editChapterEventDTOUuid :: U.UUID
  , _editChapterEventDTOPath :: EventPathDTO
  , _editChapterEventDTOChapterUuid :: U.UUID
  , _editChapterEventDTOTitle :: EventFieldDTO String
  , _editChapterEventDTOText :: EventFieldDTO String
  , _editChapterEventDTOQuestionUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _deleteChapterEventDTOUuid :: U.UUID
  , _deleteChapterEventDTOPath :: EventPathDTO
  , _deleteChapterEventDTOChapterUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- ChapterEventJM
instance FromJSON AddChapterEventDTO where
  parseJSON = simpleParseJSON "_addChapterEventDTO"

instance ToJSON AddChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_addChapterEventDTO"

instance FromJSON EditChapterEventDTO where
  parseJSON = simpleParseJSON "_editChapterEventDTO"

instance ToJSON EditChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_editChapterEventDTO"

instance FromJSON DeleteChapterEventDTO where
  parseJSON = simpleParseJSON "_deleteChapterEventDTO"

instance ToJSON DeleteChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteChapterEventDTO"

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
data EventPathItemDTO = EventPathItemDTO
  { _eventPathItemDTOPType :: String
  , _eventPathItemDTOUuid :: U.UUID
  } deriving (Show, Eq, Generic)

type EventPathDTO = [EventPathItemDTO]

-- EventPathJM
instance FromJSON EventPathItemDTO where
  parseJSON = simpleParseJSON "_eventPathItemDTO"

instance ToJSON EventPathItemDTO where
  toJSON = simpleToJSON "_eventPathItemDTO"

-- ExpertEventDTO
data AddExpertEventDTO = AddExpertEventDTO
  { _addExpertEventDTOUuid :: U.UUID
  , _addExpertEventDTOPath :: EventPathDTO
  , _addExpertEventDTOExpertUuid :: U.UUID
  , _addExpertEventDTOName :: String
  , _addExpertEventDTOEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEventDTO = EditExpertEventDTO
  { _editExpertEventDTOUuid :: U.UUID
  , _editExpertEventDTOPath :: EventPathDTO
  , _editExpertEventDTOExpertUuid :: U.UUID
  , _editExpertEventDTOName :: EventFieldDTO String
  , _editExpertEventDTOEmail :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteExpertEventDTO = DeleteExpertEventDTO
  { _deleteExpertEventDTOUuid :: U.UUID
  , _deleteExpertEventDTOPath :: EventPathDTO
  , _deleteExpertEventDTOExpertUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- ExpertEventJM
instance FromJSON AddExpertEventDTO where
  parseJSON = simpleParseJSON "_addExpertEventDTO"

instance ToJSON AddExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_addExpertEventDTO"

instance FromJSON EditExpertEventDTO where
  parseJSON = simpleParseJSON "_editExpertEventDTO"

instance ToJSON EditExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_editExpertEventDTO"

instance FromJSON DeleteExpertEventDTO where
  parseJSON = simpleParseJSON "_deleteExpertEventDTO"

instance ToJSON DeleteExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteExpertEventDTO"

-- IntegrationEventDTO
data AddIntegrationEventDTO = AddIntegrationEventDTO
  { _addIntegrationEventDTOUuid :: U.UUID
  , _addIntegrationEventDTOPath :: EventPathDTO
  , _addIntegrationEventDTOIntegrationUuid :: U.UUID
  , _addIntegrationEventDTOIId :: String
  , _addIntegrationEventDTOName :: String
  , _addIntegrationEventDTOProps :: [String]
  , _addIntegrationEventDTOLogo :: String
  , _addIntegrationEventDTORequestMethod :: String
  , _addIntegrationEventDTORequestUrl :: String
  , _addIntegrationEventDTORequestHeaders :: Map String String
  , _addIntegrationEventDTORequestBody :: String
  , _addIntegrationEventDTOResponseListField :: String
  , _addIntegrationEventDTOResponseIdField :: String
  , _addIntegrationEventDTOResponseNameField :: String
  , _addIntegrationEventDTOItemUrl :: String
  } deriving (Show, Eq, Generic)

data EditIntegrationEventDTO = EditIntegrationEventDTO
  { _editIntegrationEventDTOUuid :: U.UUID
  , _editIntegrationEventDTOPath :: EventPathDTO
  , _editIntegrationEventDTOIntegrationUuid :: U.UUID
  , _editIntegrationEventDTOIId :: EventFieldDTO String
  , _editIntegrationEventDTOName :: EventFieldDTO String
  , _editIntegrationEventDTOProps :: EventFieldDTO [String]
  , _editIntegrationEventDTOLogo :: EventFieldDTO String
  , _editIntegrationEventDTORequestMethod :: EventFieldDTO String
  , _editIntegrationEventDTORequestUrl :: EventFieldDTO String
  , _editIntegrationEventDTORequestHeaders :: EventFieldDTO (Map String String)
  , _editIntegrationEventDTORequestBody :: EventFieldDTO String
  , _editIntegrationEventDTOResponseListField :: EventFieldDTO String
  , _editIntegrationEventDTOResponseIdField :: EventFieldDTO String
  , _editIntegrationEventDTOResponseNameField :: EventFieldDTO String
  , _editIntegrationEventDTOItemUrl :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteIntegrationEventDTO = DeleteIntegrationEventDTO
  { _deleteIntegrationEventDTOUuid :: U.UUID
  , _deleteIntegrationEventDTOPath :: EventPathDTO
  , _deleteIntegrationEventDTOIntegrationUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- IntegrationEventJM
instance FromJSON AddIntegrationEventDTO where
  parseJSON = simpleParseJSON "_addIntegrationEventDTO"

instance ToJSON AddIntegrationEventDTO where
  toJSON = simpleToJSON' "eventType" "_addIntegrationEventDTO"

-- --------------------------------------------
instance FromJSON EditIntegrationEventDTO where
  parseJSON = simpleParseJSON "_editIntegrationEventDTO"

instance ToJSON EditIntegrationEventDTO where
  toJSON = simpleToJSON' "eventType" "_editIntegrationEventDTO"

-- --------------------------------------------
instance FromJSON DeleteIntegrationEventDTO where
  parseJSON = simpleParseJSON "_deleteIntegrationEventDTO"

instance ToJSON DeleteIntegrationEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteIntegrationEventDTO"

-- KnowledgeModelEventDTO
data AddKnowledgeModelEventDTO = AddKnowledgeModelEventDTO
  { _addKnowledgeModelEventDTOUuid :: U.UUID
  , _addKnowledgeModelEventDTOPath :: EventPathDTO
  , _addKnowledgeModelEventDTOKmUuid :: U.UUID
  , _addKnowledgeModelEventDTOName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO = EditKnowledgeModelEventDTO
  { _editKnowledgeModelEventDTOUuid :: U.UUID
  , _editKnowledgeModelEventDTOPath :: EventPathDTO
  , _editKnowledgeModelEventDTOKmUuid :: U.UUID
  , _editKnowledgeModelEventDTOName :: EventFieldDTO String
  , _editKnowledgeModelEventDTOChapterUuids :: EventFieldDTO [U.UUID]
  , _editKnowledgeModelEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editKnowledgeModelEventDTOIntegrationUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

-- KnowledgeModelEventJM
instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_addKnowledgeModelEventDTO"

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "eventType" "_addKnowledgeModelEventDTO"

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_editKnowledgeModelEventDTO"

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "eventType" "_editKnowledgeModelEventDTO"

-- QuestionEventDTO
data AddQuestionEventDTO
  = AddOptionsQuestionEventDTO' AddOptionsQuestionEventDTO
  | AddListQuestionEventDTO' AddListQuestionEventDTO
  | AddValueQuestionEventDTO' AddValueQuestionEventDTO
  | AddIntegrationQuestionEventDTO' AddIntegrationQuestionEventDTO
  deriving (Show, Eq, Generic)

data AddOptionsQuestionEventDTO = AddOptionsQuestionEventDTO
  { _addOptionsQuestionEventDTOUuid :: U.UUID
  , _addOptionsQuestionEventDTOPath :: EventPathDTO
  , _addOptionsQuestionEventDTOQuestionUuid :: U.UUID
  , _addOptionsQuestionEventDTOTitle :: String
  , _addOptionsQuestionEventDTOText :: Maybe String
  , _addOptionsQuestionEventDTORequiredLevel :: Maybe Int
  , _addOptionsQuestionEventDTOTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

data AddListQuestionEventDTO = AddListQuestionEventDTO
  { _addListQuestionEventDTOUuid :: U.UUID
  , _addListQuestionEventDTOPath :: EventPathDTO
  , _addListQuestionEventDTOQuestionUuid :: U.UUID
  , _addListQuestionEventDTOTitle :: String
  , _addListQuestionEventDTOText :: Maybe String
  , _addListQuestionEventDTORequiredLevel :: Maybe Int
  , _addListQuestionEventDTOTagUuids :: [U.UUID]
  , _addListQuestionEventDTOItemTemplateTitle :: String
  } deriving (Show, Eq, Generic)

data AddValueQuestionEventDTO = AddValueQuestionEventDTO
  { _addValueQuestionEventDTOUuid :: U.UUID
  , _addValueQuestionEventDTOPath :: EventPathDTO
  , _addValueQuestionEventDTOQuestionUuid :: U.UUID
  , _addValueQuestionEventDTOTitle :: String
  , _addValueQuestionEventDTOText :: Maybe String
  , _addValueQuestionEventDTORequiredLevel :: Maybe Int
  , _addValueQuestionEventDTOTagUuids :: [U.UUID]
  , _addValueQuestionEventDTOValueType :: QuestionValueType
  } deriving (Show, Eq, Generic)

data AddIntegrationQuestionEventDTO = AddIntegrationQuestionEventDTO
  { _addIntegrationQuestionEventDTOUuid :: U.UUID
  , _addIntegrationQuestionEventDTOPath :: EventPathDTO
  , _addIntegrationQuestionEventDTOQuestionUuid :: U.UUID
  , _addIntegrationQuestionEventDTOTitle :: String
  , _addIntegrationQuestionEventDTOText :: Maybe String
  , _addIntegrationQuestionEventDTORequiredLevel :: Maybe Int
  , _addIntegrationQuestionEventDTOTagUuids :: [U.UUID]
  , _addIntegrationQuestionEventDTOIntegrationUuid :: U.UUID
  , _addIntegrationQuestionEventDTOProps :: Map String String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditQuestionEventDTO
  = EditOptionsQuestionEventDTO' EditOptionsQuestionEventDTO
  | EditListQuestionEventDTO' EditListQuestionEventDTO
  | EditValueQuestionEventDTO' EditValueQuestionEventDTO
  | EditIntegrationQuestionEventDTO' EditIntegrationQuestionEventDTO
  deriving (Show, Eq, Generic)

data EditOptionsQuestionEventDTO = EditOptionsQuestionEventDTO
  { _editOptionsQuestionEventDTOUuid :: U.UUID
  , _editOptionsQuestionEventDTOPath :: EventPathDTO
  , _editOptionsQuestionEventDTOQuestionUuid :: U.UUID
  , _editOptionsQuestionEventDTOTitle :: EventFieldDTO String
  , _editOptionsQuestionEventDTOText :: EventFieldDTO (Maybe String)
  , _editOptionsQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
  , _editOptionsQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editOptionsQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
  , _editOptionsQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
  , _editOptionsQuestionEventDTOAnswerUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data EditListQuestionEventDTO = EditListQuestionEventDTO
  { _editListQuestionEventDTOUuid :: U.UUID
  , _editListQuestionEventDTOPath :: EventPathDTO
  , _editListQuestionEventDTOQuestionUuid :: U.UUID
  , _editListQuestionEventDTOTitle :: EventFieldDTO String
  , _editListQuestionEventDTOText :: EventFieldDTO (Maybe String)
  , _editListQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
  , _editListQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editListQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
  , _editListQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
  , _editListQuestionEventDTOItemTemplateTitle :: EventFieldDTO String
  , _editListQuestionEventDTOItemTemplateQuestionUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data EditValueQuestionEventDTO = EditValueQuestionEventDTO
  { _editValueQuestionEventDTOUuid :: U.UUID
  , _editValueQuestionEventDTOPath :: EventPathDTO
  , _editValueQuestionEventDTOQuestionUuid :: U.UUID
  , _editValueQuestionEventDTOTitle :: EventFieldDTO String
  , _editValueQuestionEventDTOText :: EventFieldDTO (Maybe String)
  , _editValueQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
  , _editValueQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editValueQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
  , _editValueQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
  , _editValueQuestionEventDTOValueType :: EventFieldDTO QuestionValueType
  } deriving (Show, Eq, Generic)

data EditIntegrationQuestionEventDTO = EditIntegrationQuestionEventDTO
  { _editIntegrationQuestionEventDTOUuid :: U.UUID
  , _editIntegrationQuestionEventDTOPath :: EventPathDTO
  , _editIntegrationQuestionEventDTOQuestionUuid :: U.UUID
  , _editIntegrationQuestionEventDTOTitle :: EventFieldDTO String
  , _editIntegrationQuestionEventDTOText :: EventFieldDTO (Maybe String)
  , _editIntegrationQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
  , _editIntegrationQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editIntegrationQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
  , _editIntegrationQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
  , _editIntegrationQuestionEventDTOIntegrationUuid :: EventFieldDTO U.UUID
  , _editIntegrationQuestionEventDTOProps :: EventFieldDTO (Map String String)
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteQuestionEventDTO = DeleteQuestionEventDTO
  { _deleteQuestionEventDTOUuid :: U.UUID
  , _deleteQuestionEventDTOPath :: EventPathDTO
  , _deleteQuestionEventDTOQuestionUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- QuestionEventJM
instance ToJSON AddQuestionEventDTO where
  toJSON (AddOptionsQuestionEventDTO' event) = toJSON event
  toJSON (AddListQuestionEventDTO' event) = toJSON event
  toJSON (AddValueQuestionEventDTO' event) = toJSON event
  toJSON (AddIntegrationQuestionEventDTO' event) = toJSON event

instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "questionType"
    case referenceType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEventDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEventDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEventDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (AddIntegrationQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddOptionsQuestionEventDTO where
  parseJSON = simpleParseJSON "_addOptionsQuestionEventDTO"

instance ToJSON AddOptionsQuestionEventDTO where
  toJSON AddOptionsQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "questionType" .= "OptionsQuestion"
      , "uuid" .= _addOptionsQuestionEventDTOUuid
      , "path" .= _addOptionsQuestionEventDTOPath
      , "questionUuid" .= _addOptionsQuestionEventDTOQuestionUuid
      , "title" .= _addOptionsQuestionEventDTOTitle
      , "text" .= _addOptionsQuestionEventDTOText
      , "requiredLevel" .= _addOptionsQuestionEventDTORequiredLevel
      , "tagUuids" .= _addOptionsQuestionEventDTOTagUuids
      ]

-- --------------------------------------------
instance FromJSON AddListQuestionEventDTO where
  parseJSON = simpleParseJSON "_addListQuestionEventDTO"

instance ToJSON AddListQuestionEventDTO where
  toJSON AddListQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "questionType" .= "ListQuestion"
      , "uuid" .= _addListQuestionEventDTOUuid
      , "path" .= _addListQuestionEventDTOPath
      , "questionUuid" .= _addListQuestionEventDTOQuestionUuid
      , "title" .= _addListQuestionEventDTOTitle
      , "text" .= _addListQuestionEventDTOText
      , "requiredLevel" .= _addListQuestionEventDTORequiredLevel
      , "tagUuids" .= _addListQuestionEventDTOTagUuids
      , "itemTemplateTitle" .= _addListQuestionEventDTOItemTemplateTitle
      ]

-- --------------------------------------------
instance FromJSON AddValueQuestionEventDTO where
  parseJSON = simpleParseJSON "_addValueQuestionEventDTO"

instance ToJSON AddValueQuestionEventDTO where
  toJSON AddValueQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "questionType" .= "ValueQuestion"
      , "uuid" .= _addValueQuestionEventDTOUuid
      , "path" .= _addValueQuestionEventDTOPath
      , "questionUuid" .= _addValueQuestionEventDTOQuestionUuid
      , "title" .= _addValueQuestionEventDTOTitle
      , "text" .= _addValueQuestionEventDTOText
      , "requiredLevel" .= _addValueQuestionEventDTORequiredLevel
      , "tagUuids" .= _addValueQuestionEventDTOTagUuids
      , "valueType" .= _addValueQuestionEventDTOValueType
      ]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEventDTO where
  parseJSON = simpleParseJSON "_addIntegrationQuestionEventDTO"

instance ToJSON AddIntegrationQuestionEventDTO where
  toJSON AddIntegrationQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "questionType" .= "IntegrationQuestion"
      , "uuid" .= _addIntegrationQuestionEventDTOUuid
      , "path" .= _addIntegrationQuestionEventDTOPath
      , "questionUuid" .= _addIntegrationQuestionEventDTOQuestionUuid
      , "title" .= _addIntegrationQuestionEventDTOTitle
      , "text" .= _addIntegrationQuestionEventDTOText
      , "requiredLevel" .= _addIntegrationQuestionEventDTORequiredLevel
      , "tagUuids" .= _addIntegrationQuestionEventDTOTagUuids
      , "integrationUuid" .= _addIntegrationQuestionEventDTOIntegrationUuid
      , "props" .= _addIntegrationQuestionEventDTOProps
      ]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditQuestionEventDTO where
  toJSON (EditOptionsQuestionEventDTO' event) = toJSON event
  toJSON (EditListQuestionEventDTO' event) = toJSON event
  toJSON (EditValueQuestionEventDTO' event) = toJSON event
  toJSON (EditIntegrationQuestionEventDTO' event) = toJSON event

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEventDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEventDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEventDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (EditIntegrationQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditOptionsQuestionEventDTO where
  parseJSON = simpleParseJSON "_editOptionsQuestionEventDTO"

instance ToJSON EditOptionsQuestionEventDTO where
  toJSON EditOptionsQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "questionType" .= "OptionsQuestion"
      , "uuid" .= _editOptionsQuestionEventDTOUuid
      , "path" .= _editOptionsQuestionEventDTOPath
      , "questionUuid" .= _editOptionsQuestionEventDTOQuestionUuid
      , "title" .= _editOptionsQuestionEventDTOTitle
      , "text" .= _editOptionsQuestionEventDTOText
      , "requiredLevel" .= _editOptionsQuestionEventDTORequiredLevel
      , "tagUuids" .= _editOptionsQuestionEventDTOTagUuids
      , "expertUuids" .= _editOptionsQuestionEventDTOExpertUuids
      , "referenceUuids" .= _editOptionsQuestionEventDTOReferenceUuids
      , "answerUuids" .= _editOptionsQuestionEventDTOAnswerUuids
      ]

-- --------------------------------------------
instance FromJSON EditListQuestionEventDTO where
  parseJSON = simpleParseJSON "_editListQuestionEventDTO"

instance ToJSON EditListQuestionEventDTO where
  toJSON EditListQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "questionType" .= "ListQuestion"
      , "uuid" .= _editListQuestionEventDTOUuid
      , "path" .= _editListQuestionEventDTOPath
      , "questionUuid" .= _editListQuestionEventDTOQuestionUuid
      , "title" .= _editListQuestionEventDTOTitle
      , "text" .= _editListQuestionEventDTOText
      , "requiredLevel" .= _editListQuestionEventDTORequiredLevel
      , "tagUuids" .= _editListQuestionEventDTOTagUuids
      , "expertUuids" .= _editListQuestionEventDTOExpertUuids
      , "referenceUuids" .= _editListQuestionEventDTOReferenceUuids
      , "itemTemplateTitle" .= _editListQuestionEventDTOItemTemplateTitle
      , "itemTemplateQuestionUuids" .= _editListQuestionEventDTOItemTemplateQuestionUuids
      ]

-- --------------------------------------------
instance FromJSON EditValueQuestionEventDTO where
  parseJSON = simpleParseJSON "_editValueQuestionEventDTO"

instance ToJSON EditValueQuestionEventDTO where
  toJSON EditValueQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "questionType" .= "ValueQuestion"
      , "uuid" .= _editValueQuestionEventDTOUuid
      , "path" .= _editValueQuestionEventDTOPath
      , "questionUuid" .= _editValueQuestionEventDTOQuestionUuid
      , "title" .= _editValueQuestionEventDTOTitle
      , "text" .= _editValueQuestionEventDTOText
      , "requiredLevel" .= _editValueQuestionEventDTORequiredLevel
      , "tagUuids" .= _editValueQuestionEventDTOTagUuids
      , "expertUuids" .= _editValueQuestionEventDTOExpertUuids
      , "referenceUuids" .= _editValueQuestionEventDTOReferenceUuids
      , "valueType" .= _editValueQuestionEventDTOValueType
      ]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEventDTO where
  parseJSON = simpleParseJSON "_editIntegrationQuestionEventDTO"

instance ToJSON EditIntegrationQuestionEventDTO where
  toJSON EditIntegrationQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "questionType" .= "IntegrationQuestion"
      , "uuid" .= _editIntegrationQuestionEventDTOUuid
      , "path" .= _editIntegrationQuestionEventDTOPath
      , "questionUuid" .= _editIntegrationQuestionEventDTOQuestionUuid
      , "title" .= _editIntegrationQuestionEventDTOTitle
      , "text" .= _editIntegrationQuestionEventDTOText
      , "requiredLevel" .= _editIntegrationQuestionEventDTORequiredLevel
      , "tagUuids" .= _editIntegrationQuestionEventDTOTagUuids
      , "expertUuids" .= _editIntegrationQuestionEventDTOExpertUuids
      , "referenceUuids" .= _editIntegrationQuestionEventDTOReferenceUuids
      , "integrationUuid" .= _editIntegrationQuestionEventDTOIntegrationUuid
      , "props" .= _editIntegrationQuestionEventDTOProps
      ]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteQuestionEventDTO where
  parseJSON = simpleParseJSON "_deleteQuestionEventDTO"

instance ToJSON DeleteQuestionEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteQuestionEventDTO"

-- ReferenceEventDTO
data AddReferenceEventDTO
  = AddResourcePageReferenceEventDTO' AddResourcePageReferenceEventDTO
  | AddURLReferenceEventDTO' AddURLReferenceEventDTO
  | AddCrossReferenceEventDTO' AddCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEventDTO = AddResourcePageReferenceEventDTO
  { _addResourcePageReferenceEventDTOUuid :: U.UUID
  , _addResourcePageReferenceEventDTOPath :: EventPathDTO
  , _addResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  , _addResourcePageReferenceEventDTOShortUuid :: String
  } deriving (Show, Eq, Generic)

data AddURLReferenceEventDTO = AddURLReferenceEventDTO
  { _addURLReferenceEventDTOUuid :: U.UUID
  , _addURLReferenceEventDTOPath :: EventPathDTO
  , _addURLReferenceEventDTOReferenceUuid :: U.UUID
  , _addURLReferenceEventDTOUrl :: String
  , _addURLReferenceEventDTOLabel :: String
  } deriving (Show, Eq, Generic)

data AddCrossReferenceEventDTO = AddCrossReferenceEventDTO
  { _addCrossReferenceEventDTOUuid :: U.UUID
  , _addCrossReferenceEventDTOPath :: EventPathDTO
  , _addCrossReferenceEventDTOReferenceUuid :: U.UUID
  , _addCrossReferenceEventDTOTargetUuid :: U.UUID
  , _addCrossReferenceEventDTODescription :: String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditReferenceEventDTO
  = EditResourcePageReferenceEventDTO' EditResourcePageReferenceEventDTO
  | EditURLReferenceEventDTO' EditURLReferenceEventDTO
  | EditCrossReferenceEventDTO' EditCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data EditResourcePageReferenceEventDTO = EditResourcePageReferenceEventDTO
  { _editResourcePageReferenceEventDTOUuid :: U.UUID
  , _editResourcePageReferenceEventDTOPath :: EventPathDTO
  , _editResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  , _editResourcePageReferenceEventDTOShortUuid :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data EditURLReferenceEventDTO = EditURLReferenceEventDTO
  { _editURLReferenceEventDTOUuid :: U.UUID
  , _editURLReferenceEventDTOPath :: EventPathDTO
  , _editURLReferenceEventDTOReferenceUuid :: U.UUID
  , _editURLReferenceEventDTOUrl :: EventFieldDTO String
  , _editURLReferenceEventDTOLabel :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data EditCrossReferenceEventDTO = EditCrossReferenceEventDTO
  { _editCrossReferenceEventDTOUuid :: U.UUID
  , _editCrossReferenceEventDTOPath :: EventPathDTO
  , _editCrossReferenceEventDTOReferenceUuid :: U.UUID
  , _editCrossReferenceEventDTOTargetUuid :: EventFieldDTO U.UUID
  , _editCrossReferenceEventDTODescription :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEventDTO = DeleteReferenceEventDTO
  { _deleteReferenceEventDTOUuid :: U.UUID
  , _deleteReferenceEventDTOPath :: EventPathDTO
  , _deleteReferenceEventDTOReferenceUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- ReferenceEventJM
instance ToJSON AddReferenceEventDTO where
  toJSON (AddResourcePageReferenceEventDTO' event) = toJSON event
  toJSON (AddURLReferenceEventDTO' event) = toJSON event
  toJSON (AddCrossReferenceEventDTO' event) = toJSON event

instance FromJSON AddReferenceEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (AddResourcePageReferenceEventDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (AddURLReferenceEventDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (AddCrossReferenceEventDTO' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddResourcePageReferenceEventDTO where
  parseJSON = simpleParseJSON "_addResourcePageReferenceEventDTO"

instance ToJSON AddResourcePageReferenceEventDTO where
  toJSON AddResourcePageReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "referenceType" .= "ResourcePageReference"
      , "uuid" .= _addResourcePageReferenceEventDTOUuid
      , "path" .= _addResourcePageReferenceEventDTOPath
      , "referenceUuid" .= _addResourcePageReferenceEventDTOReferenceUuid
      , "shortUuid" .= _addResourcePageReferenceEventDTOShortUuid
      ]

-- --------------------------------------------
instance FromJSON AddURLReferenceEventDTO where
  parseJSON = simpleParseJSON "_addURLReferenceEventDTO"

instance ToJSON AddURLReferenceEventDTO where
  toJSON AddURLReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "referenceType" .= "URLReference"
      , "uuid" .= _addURLReferenceEventDTOUuid
      , "path" .= _addURLReferenceEventDTOPath
      , "referenceUuid" .= _addURLReferenceEventDTOReferenceUuid
      , "url" .= _addURLReferenceEventDTOUrl
      , "label" .= _addURLReferenceEventDTOLabel
      ]

-- --------------------------------------------
instance FromJSON AddCrossReferenceEventDTO where
  parseJSON = simpleParseJSON "_addCrossReferenceEventDTO"

instance ToJSON AddCrossReferenceEventDTO where
  toJSON AddCrossReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "referenceType" .= "CrossReference"
      , "uuid" .= _addCrossReferenceEventDTOUuid
      , "path" .= _addCrossReferenceEventDTOPath
      , "referenceUuid" .= _addCrossReferenceEventDTOReferenceUuid
      , "targetUuid" .= _addCrossReferenceEventDTOTargetUuid
      , "description" .= _addCrossReferenceEventDTODescription
      ]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditReferenceEventDTO where
  toJSON (EditResourcePageReferenceEventDTO' event) = toJSON event
  toJSON (EditURLReferenceEventDTO' event) = toJSON event
  toJSON (EditCrossReferenceEventDTO' event) = toJSON event

instance FromJSON EditReferenceEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (EditResourcePageReferenceEventDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (EditURLReferenceEventDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (EditCrossReferenceEventDTO' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditResourcePageReferenceEventDTO where
  parseJSON = simpleParseJSON "_editResourcePageReferenceEventDTO"

instance ToJSON EditResourcePageReferenceEventDTO where
  toJSON EditResourcePageReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "referenceType" .= "ResourcePageReference"
      , "uuid" .= _editResourcePageReferenceEventDTOUuid
      , "path" .= _editResourcePageReferenceEventDTOPath
      , "referenceUuid" .= _editResourcePageReferenceEventDTOReferenceUuid
      , "shortUuid" .= _editResourcePageReferenceEventDTOShortUuid
      ]

-- --------------------------------------------
instance FromJSON EditURLReferenceEventDTO where
  parseJSON = simpleParseJSON "_editURLReferenceEventDTO"

instance ToJSON EditURLReferenceEventDTO where
  toJSON EditURLReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "referenceType" .= "URLReference"
      , "uuid" .= _editURLReferenceEventDTOUuid
      , "path" .= _editURLReferenceEventDTOPath
      , "referenceUuid" .= _editURLReferenceEventDTOReferenceUuid
      , "url" .= _editURLReferenceEventDTOUrl
      , "label" .= _editURLReferenceEventDTOLabel
      ]

-- --------------------------------------------
instance FromJSON EditCrossReferenceEventDTO where
  parseJSON = simpleParseJSON "_editCrossReferenceEventDTO"

instance ToJSON EditCrossReferenceEventDTO where
  toJSON EditCrossReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "referenceType" .= "CrossReference"
      , "uuid" .= _editCrossReferenceEventDTOUuid
      , "path" .= _editCrossReferenceEventDTOPath
      , "referenceUuid" .= _editCrossReferenceEventDTOReferenceUuid
      , "targetUuid" .= _editCrossReferenceEventDTOTargetUuid
      , "description" .= _editCrossReferenceEventDTODescription
      ]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteReferenceEventDTO where
  parseJSON = simpleParseJSON "_deleteReferenceEventDTO"

instance ToJSON DeleteReferenceEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteReferenceEventDTO"

-- TagEventDTO
data AddTagEventDTO = AddTagEventDTO
  { _addTagEventDTOUuid :: U.UUID
  , _addTagEventDTOPath :: EventPathDTO
  , _addTagEventDTOTagUuid :: U.UUID
  , _addTagEventDTOName :: String
  , _addTagEventDTODescription :: Maybe String
  , _addTagEventDTOColor :: String
  } deriving (Show, Eq, Generic)

data EditTagEventDTO = EditTagEventDTO
  { _editTagEventDTOUuid :: U.UUID
  , _editTagEventDTOPath :: EventPathDTO
  , _editTagEventDTOTagUuid :: U.UUID
  , _editTagEventDTOName :: EventFieldDTO String
  , _editTagEventDTODescription :: EventFieldDTO (Maybe String)
  , _editTagEventDTOColor :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteTagEventDTO = DeleteTagEventDTO
  { _deleteTagEventDTOUuid :: U.UUID
  , _deleteTagEventDTOPath :: EventPathDTO
  , _deleteTagEventDTOTagUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- TagEventJM
instance FromJSON AddTagEventDTO where
  parseJSON = simpleParseJSON "_addTagEventDTO"

instance ToJSON AddTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_addTagEventDTO"

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON = simpleParseJSON "_editTagEventDTO"

instance ToJSON EditTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_editTagEventDTO"

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON = simpleParseJSON "_deleteTagEventDTO"

instance ToJSON DeleteTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteTagEventDTO"

-- KnowledgeModelDTO
data MetricMeasureDTO = MetricMeasureDTO
  { _metricMeasureDTOMetricUuid :: U.UUID
  , _metricMeasureDTOMeasure :: Double
  , _metricMeasureDTOWeight :: Double
  } deriving (Show, Eq)

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
    _ -> Left $ "Value is not object"

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
stripSuffix suffix string = T.unpack <$> (T.stripSuffix (T.pack suffix) (T.pack string))
