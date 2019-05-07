module DSW.Metamodel.Event.Version2 where

import Control.Monad
import Data.Aeson
import Data.Map
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

-- Created from dsw-server @ba78aef208e899807fd1e46a5615df7a53904d82
-- * Api.Resource.Common
-- * Api.Resource.Event.EventDTO
-- * Api.Resource.Event.EventFieldDTO
-- * Api.Resource.Event.EventPathDTO
-- * Api.Resource.KnowledgeModel.KnowledgeModelDTO
-- * Model.KnowledgeModel.KnowledgeModel

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

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
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

-- -------------------------
-- Chapter -----------------
-- -------------------------
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

-- -------------------------
-- Question ----------------
-- -------------------------
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

-- -------------------------
-- Answer ------------------
-- -------------------------
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

-- -------------------------
-- Expert ------------------
-- -------------------------
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

-- -------------------------
-- Reference ---------------
-- -------------------------
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

-- -------------------------
-- Tag ---------------------
-- -------------------------
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

-- -------------------------
-- Integration -------------
-- -------------------------
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

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _addKnowledgeModelEventDTOUuid <- o .: "uuid"
    _addKnowledgeModelEventDTOPath <- o .: "path"
    _addKnowledgeModelEventDTOKmUuid <- o .: "kmUuid"
    _addKnowledgeModelEventDTOName <- o .: "name"
    return AddKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON AddKnowledgeModelEventDTO {..} =
    object
      [ "eventType" .= "AddKnowledgeModelEvent"
      , "uuid" .= _addKnowledgeModelEventDTOUuid
      , "path" .= _addKnowledgeModelEventDTOPath
      , "kmUuid" .= _addKnowledgeModelEventDTOKmUuid
      , "name" .= _addKnowledgeModelEventDTOName
      ]

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _editKnowledgeModelEventDTOUuid <- o .: "uuid"
    _editKnowledgeModelEventDTOPath <- o .: "path"
    _editKnowledgeModelEventDTOKmUuid <- o .: "kmUuid"
    _editKnowledgeModelEventDTOName <- o .: "name"
    _editKnowledgeModelEventDTOChapterUuids <- o .: "chapterUuids"
    _editKnowledgeModelEventDTOTagUuids <- o .: "tagUuids"
    _editKnowledgeModelEventDTOIntegrationUuids <- o .: "integrationUuids"
    return EditKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON EditKnowledgeModelEventDTO {..} =
    object
      [ "eventType" .= "EditKnowledgeModelEvent"
      , "uuid" .= _editKnowledgeModelEventDTOUuid
      , "path" .= _editKnowledgeModelEventDTOPath
      , "kmUuid" .= _editKnowledgeModelEventDTOKmUuid
      , "name" .= _editKnowledgeModelEventDTOName
      , "chapterUuids" .= _editKnowledgeModelEventDTOChapterUuids
      , "tagUuids" .= _editKnowledgeModelEventDTOTagUuids
      , "integrationUuids" .= _editKnowledgeModelEventDTOIntegrationUuids
      ]

-------------------------
-- Chapter --------------
-------------------------
instance FromJSON AddChapterEventDTO where
  parseJSON (Object o) = do
    _addChapterEventDTOUuid <- o .: "uuid"
    _addChapterEventDTOPath <- o .: "path"
    _addChapterEventDTOChapterUuid <- o .: "chapterUuid"
    _addChapterEventDTOTitle <- o .: "title"
    _addChapterEventDTOText <- o .: "text"
    return AddChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddChapterEventDTO where
  toJSON AddChapterEventDTO {..} =
    object
      [ "eventType" .= "AddChapterEvent"
      , "uuid" .= _addChapterEventDTOUuid
      , "path" .= _addChapterEventDTOPath
      , "chapterUuid" .= _addChapterEventDTOChapterUuid
      , "title" .= _addChapterEventDTOTitle
      , "text" .= _addChapterEventDTOText
      ]

instance FromJSON EditChapterEventDTO where
  parseJSON (Object o) = do
    _editChapterEventDTOUuid <- o .: "uuid"
    _editChapterEventDTOPath <- o .: "path"
    _editChapterEventDTOChapterUuid <- o .: "chapterUuid"
    _editChapterEventDTOTitle <- o .: "title"
    _editChapterEventDTOText <- o .: "text"
    _editChapterEventDTOQuestionUuids <- o .: "questionUuids"
    return EditChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditChapterEventDTO where
  toJSON EditChapterEventDTO {..} =
    object
      [ "eventType" .= "EditChapterEvent"
      , "uuid" .= _editChapterEventDTOUuid
      , "path" .= _editChapterEventDTOPath
      , "chapterUuid" .= _editChapterEventDTOChapterUuid
      , "title" .= _editChapterEventDTOTitle
      , "text" .= _editChapterEventDTOText
      , "questionUuids" .= _editChapterEventDTOQuestionUuids
      ]

instance FromJSON DeleteChapterEventDTO where
  parseJSON (Object o) = do
    _deleteChapterEventDTOUuid <- o .: "uuid"
    _deleteChapterEventDTOPath <- o .: "path"
    _deleteChapterEventDTOChapterUuid <- o .: "chapterUuid"
    return DeleteChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteChapterEventDTO where
  toJSON DeleteChapterEventDTO {..} =
    object
      [ "eventType" .= "DeleteChapterEvent"
      , "uuid" .= _deleteChapterEventDTOUuid
      , "path" .= _deleteChapterEventDTOPath
      , "chapterUuid" .= _deleteChapterEventDTOChapterUuid
      ]

-- -------------------------
-- Question ----------------
-- -------------------------
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
  parseJSON (Object o) = do
    _addOptionsQuestionEventDTOUuid <- o .: "uuid"
    _addOptionsQuestionEventDTOPath <- o .: "path"
    _addOptionsQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addOptionsQuestionEventDTOTitle <- o .: "title"
    _addOptionsQuestionEventDTOText <- o .: "text"
    _addOptionsQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _addOptionsQuestionEventDTOTagUuids <- o .: "tagUuids"
    return AddOptionsQuestionEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _addListQuestionEventDTOUuid <- o .: "uuid"
    _addListQuestionEventDTOPath <- o .: "path"
    _addListQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addListQuestionEventDTOTitle <- o .: "title"
    _addListQuestionEventDTOText <- o .: "text"
    _addListQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _addListQuestionEventDTOTagUuids <- o .: "tagUuids"
    _addListQuestionEventDTOItemTemplateTitle <- o .: "itemTemplateTitle"
    return AddListQuestionEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _addValueQuestionEventDTOUuid <- o .: "uuid"
    _addValueQuestionEventDTOPath <- o .: "path"
    _addValueQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addValueQuestionEventDTOTitle <- o .: "title"
    _addValueQuestionEventDTOText <- o .: "text"
    _addValueQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _addValueQuestionEventDTOTagUuids <- o .: "tagUuids"
    valueType <- o .: "valueType"
    case deserializeQuestionValueType valueType of
      (Just _addValueQuestionEventDTOValueType) -> return AddValueQuestionEventDTO {..}
      Nothing -> fail "Unsupported question value type"
  parseJSON _ = mzero

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
      , "valueType" .= serializeQuestionValueType _addValueQuestionEventDTOValueType
      ]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEventDTO where
  parseJSON (Object o) = do
    _addIntegrationQuestionEventDTOUuid <- o .: "uuid"
    _addIntegrationQuestionEventDTOPath <- o .: "path"
    _addIntegrationQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addIntegrationQuestionEventDTOTitle <- o .: "title"
    _addIntegrationQuestionEventDTOText <- o .: "text"
    _addIntegrationQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _addIntegrationQuestionEventDTOTagUuids <- o .: "tagUuids"
    _addIntegrationQuestionEventDTOIntegrationUuid <- o .: "integrationUuid"
    _addIntegrationQuestionEventDTOProps <- o .: "props"
    return AddIntegrationQuestionEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _editOptionsQuestionEventDTOUuid <- o .: "uuid"
    _editOptionsQuestionEventDTOPath <- o .: "path"
    _editOptionsQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editOptionsQuestionEventDTOTitle <- o .: "title"
    _editOptionsQuestionEventDTOText <- o .: "text"
    _editOptionsQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _editOptionsQuestionEventDTOTagUuids <- o .: "tagUuids"
    _editOptionsQuestionEventDTOExpertUuids <- o .: "expertUuids"
    _editOptionsQuestionEventDTOReferenceUuids <- o .: "referenceUuids"
    _editOptionsQuestionEventDTOAnswerUuids <- o .: "answerUuids"
    return EditOptionsQuestionEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _editListQuestionEventDTOUuid <- o .: "uuid"
    _editListQuestionEventDTOPath <- o .: "path"
    _editListQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editListQuestionEventDTOTitle <- o .: "title"
    _editListQuestionEventDTOText <- o .: "text"
    _editListQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _editListQuestionEventDTOTagUuids <- o .: "tagUuids"
    _editListQuestionEventDTOExpertUuids <- o .: "expertUuids"
    _editListQuestionEventDTOReferenceUuids <- o .: "referenceUuids"
    _editListQuestionEventDTOItemTemplateTitle <- o .: "itemTemplateTitle"
    _editListQuestionEventDTOItemTemplateQuestionUuids <- o .: "itemTemplateQuestionUuids"
    return EditListQuestionEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _editValueQuestionEventDTOUuid <- o .: "uuid"
    _editValueQuestionEventDTOPath <- o .: "path"
    _editValueQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editValueQuestionEventDTOTitle <- o .: "title"
    _editValueQuestionEventDTOText <- o .: "text"
    _editValueQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _editValueQuestionEventDTOTagUuids <- o .: "tagUuids"
    _editValueQuestionEventDTOExpertUuids <- o .: "expertUuids"
    _editValueQuestionEventDTOReferenceUuids <- o .: "referenceUuids"
    valueType <- o .: "valueType"
    case deserializeEventFieldQuestionValueType <$> valueType of
      (Just _editValueQuestionEventDTOValueType) -> return EditValueQuestionEventDTO {..}
      Nothing -> fail "Unsupported question value type"
  parseJSON _ = mzero

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
      , "valueType" .= (serializeQuestionValueType <$> _editValueQuestionEventDTOValueType)
      ]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEventDTO where
  parseJSON (Object o) = do
    _editIntegrationQuestionEventDTOUuid <- o .: "uuid"
    _editIntegrationQuestionEventDTOPath <- o .: "path"
    _editIntegrationQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editIntegrationQuestionEventDTOTitle <- o .: "title"
    _editIntegrationQuestionEventDTOText <- o .: "text"
    _editIntegrationQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _editIntegrationQuestionEventDTOTagUuids <- o .: "tagUuids"
    _editIntegrationQuestionEventDTOExpertUuids <- o .: "expertUuids"
    _editIntegrationQuestionEventDTOReferenceUuids <- o .: "referenceUuids"
    _editIntegrationQuestionEventDTOIntegrationUuid <- o .: "integrationUuid"
    _editIntegrationQuestionEventDTOProps <- o .: "props"
    return EditIntegrationQuestionEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _deleteQuestionEventDTOUuid <- o .: "uuid"
    _deleteQuestionEventDTOPath <- o .: "path"
    _deleteQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    return DeleteQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteQuestionEventDTO where
  toJSON DeleteQuestionEventDTO {..} =
    object
      [ "eventType" .= "DeleteQuestionEvent"
      , "uuid" .= _deleteQuestionEventDTOUuid
      , "path" .= _deleteQuestionEventDTOPath
      , "questionUuid" .= _deleteQuestionEventDTOQuestionUuid
      ]

-- -------------------------
-- Answer ------------------
-- -------------------------
instance FromJSON AddAnswerEventDTO where
  parseJSON (Object o) = do
    _addAnswerEventDTOUuid <- o .: "uuid"
    _addAnswerEventDTOPath <- o .: "path"
    _addAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    _addAnswerEventDTOLabel <- o .: "label"
    _addAnswerEventDTOAdvice <- o .: "advice"
    _addAnswerEventDTOMetricMeasures <- o .: "metricMeasures"
    return AddAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddAnswerEventDTO where
  toJSON AddAnswerEventDTO {..} =
    object
      [ "eventType" .= "AddAnswerEvent"
      , "uuid" .= _addAnswerEventDTOUuid
      , "path" .= _addAnswerEventDTOPath
      , "answerUuid" .= _addAnswerEventDTOAnswerUuid
      , "label" .= _addAnswerEventDTOLabel
      , "advice" .= _addAnswerEventDTOAdvice
      , "metricMeasures" .= _addAnswerEventDTOMetricMeasures
      ]

instance FromJSON EditAnswerEventDTO where
  parseJSON (Object o) = do
    _editAnswerEventDTOUuid <- o .: "uuid"
    _editAnswerEventDTOPath <- o .: "path"
    _editAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    _editAnswerEventDTOLabel <- o .: "label"
    _editAnswerEventDTOAdvice <- o .: "advice"
    _editAnswerEventDTOFollowUpUuids <- o .: "followUpUuids"
    _editAnswerEventDTOMetricMeasures <- o .: "metricMeasures"
    return EditAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditAnswerEventDTO where
  toJSON EditAnswerEventDTO {..} =
    object
      [ "eventType" .= "EditAnswerEvent"
      , "uuid" .= _editAnswerEventDTOUuid
      , "path" .= _editAnswerEventDTOPath
      , "answerUuid" .= _editAnswerEventDTOAnswerUuid
      , "label" .= _editAnswerEventDTOLabel
      , "advice" .= _editAnswerEventDTOAdvice
      , "followUpUuids" .= _editAnswerEventDTOFollowUpUuids
      , "metricMeasures" .= _editAnswerEventDTOMetricMeasures
      ]

instance FromJSON DeleteAnswerEventDTO where
  parseJSON (Object o) = do
    _deleteAnswerEventDTOUuid <- o .: "uuid"
    _deleteAnswerEventDTOPath <- o .: "path"
    _deleteAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    return DeleteAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteAnswerEventDTO where
  toJSON DeleteAnswerEventDTO {..} =
    object
      [ "eventType" .= "DeleteAnswerEvent"
      , "uuid" .= _deleteAnswerEventDTOUuid
      , "path" .= _deleteAnswerEventDTOPath
      , "answerUuid" .= _deleteAnswerEventDTOAnswerUuid
      ]

-- -------------------------
-- Expert ------------------
-- -------------------------
instance FromJSON AddExpertEventDTO where
  parseJSON (Object o) = do
    _addExpertEventDTOUuid <- o .: "uuid"
    _addExpertEventDTOPath <- o .: "path"
    _addExpertEventDTOExpertUuid <- o .: "expertUuid"
    _addExpertEventDTOName <- o .: "name"
    _addExpertEventDTOEmail <- o .: "email"
    return AddExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddExpertEventDTO where
  toJSON AddExpertEventDTO {..} =
    object
      [ "eventType" .= "AddExpertEvent"
      , "uuid" .= _addExpertEventDTOUuid
      , "path" .= _addExpertEventDTOPath
      , "expertUuid" .= _addExpertEventDTOExpertUuid
      , "name" .= _addExpertEventDTOName
      , "email" .= _addExpertEventDTOEmail
      ]

instance FromJSON EditExpertEventDTO where
  parseJSON (Object o) = do
    _editExpertEventDTOUuid <- o .: "uuid"
    _editExpertEventDTOPath <- o .: "path"
    _editExpertEventDTOExpertUuid <- o .: "expertUuid"
    _editExpertEventDTOName <- o .: "name"
    _editExpertEventDTOEmail <- o .: "email"
    return EditExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditExpertEventDTO where
  toJSON EditExpertEventDTO {..} =
    object
      [ "eventType" .= "EditExpertEvent"
      , "uuid" .= _editExpertEventDTOUuid
      , "path" .= _editExpertEventDTOPath
      , "expertUuid" .= _editExpertEventDTOExpertUuid
      , "name" .= _editExpertEventDTOName
      , "email" .= _editExpertEventDTOEmail
      ]

instance FromJSON DeleteExpertEventDTO where
  parseJSON (Object o) = do
    _deleteExpertEventDTOUuid <- o .: "uuid"
    _deleteExpertEventDTOPath <- o .: "path"
    _deleteExpertEventDTOExpertUuid <- o .: "expertUuid"
    return DeleteExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteExpertEventDTO where
  toJSON DeleteExpertEventDTO {..} =
    object
      [ "eventType" .= "DeleteExpertEvent"
      , "uuid" .= _deleteExpertEventDTOUuid
      , "path" .= _deleteExpertEventDTOPath
      , "expertUuid" .= _deleteExpertEventDTOExpertUuid
      ]

-- -------------------------
-- Reference ---------------
-- -------------------------
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
  parseJSON (Object o) = do
    _addResourcePageReferenceEventDTOUuid <- o .: "uuid"
    _addResourcePageReferenceEventDTOPath <- o .: "path"
    _addResourcePageReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _addResourcePageReferenceEventDTOShortUuid <- o .: "shortUuid"
    return AddResourcePageReferenceEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _addURLReferenceEventDTOUuid <- o .: "uuid"
    _addURLReferenceEventDTOPath <- o .: "path"
    _addURLReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _addURLReferenceEventDTOUrl <- o .: "url"
    _addURLReferenceEventDTOLabel <- o .: "label"
    return AddURLReferenceEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _addCrossReferenceEventDTOUuid <- o .: "uuid"
    _addCrossReferenceEventDTOPath <- o .: "path"
    _addCrossReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _addCrossReferenceEventDTOTargetUuid <- o .: "targetUuid"
    _addCrossReferenceEventDTODescription <- o .: "description"
    return AddCrossReferenceEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _editResourcePageReferenceEventDTOUuid <- o .: "uuid"
    _editResourcePageReferenceEventDTOPath <- o .: "path"
    _editResourcePageReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _editResourcePageReferenceEventDTOShortUuid <- o .: "shortUuid"
    return EditResourcePageReferenceEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _editURLReferenceEventDTOUuid <- o .: "uuid"
    _editURLReferenceEventDTOPath <- o .: "path"
    _editURLReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _editURLReferenceEventDTOUrl <- o .: "url"
    _editURLReferenceEventDTOLabel <- o .: "label"
    return EditURLReferenceEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _editCrossReferenceEventDTOUuid <- o .: "uuid"
    _editCrossReferenceEventDTOPath <- o .: "path"
    _editCrossReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _editCrossReferenceEventDTOTargetUuid <- o .: "targetUuid"
    _editCrossReferenceEventDTODescription <- o .: "description"
    return EditCrossReferenceEventDTO {..}
  parseJSON _ = mzero

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
  parseJSON (Object o) = do
    _deleteReferenceEventDTOUuid <- o .: "uuid"
    _deleteReferenceEventDTOPath <- o .: "path"
    _deleteReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    return DeleteReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteReferenceEventDTO where
  toJSON DeleteReferenceEventDTO {..} =
    object
      [ "eventType" .= "DeleteReferenceEvent"
      , "uuid" .= _deleteReferenceEventDTOUuid
      , "path" .= _deleteReferenceEventDTOPath
      , "referenceUuid" .= _deleteReferenceEventDTOReferenceUuid
      ]

-- -------------------------
-- Tag ---------------------
-- -------------------------
instance FromJSON AddTagEventDTO where
  parseJSON (Object o) = do
    _addTagEventDTOUuid <- o .: "uuid"
    _addTagEventDTOPath <- o .: "path"
    _addTagEventDTOTagUuid <- o .: "tagUuid"
    _addTagEventDTOName <- o .: "name"
    _addTagEventDTODescription <- o .: "description"
    _addTagEventDTOColor <- o .: "color"
    return AddTagEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddTagEventDTO where
  toJSON AddTagEventDTO {..} =
    object
      [ "eventType" .= "AddTagEvent"
      , "uuid" .= _addTagEventDTOUuid
      , "path" .= _addTagEventDTOPath
      , "tagUuid" .= _addTagEventDTOTagUuid
      , "name" .= _addTagEventDTOName
      , "description" .= _addTagEventDTODescription
      , "color" .= _addTagEventDTOColor
      ]

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON (Object o) = do
    _editTagEventDTOUuid <- o .: "uuid"
    _editTagEventDTOPath <- o .: "path"
    _editTagEventDTOTagUuid <- o .: "tagUuid"
    _editTagEventDTOName <- o .: "name"
    _editTagEventDTODescription <- o .: "description"
    _editTagEventDTOColor <- o .: "color"
    return EditTagEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditTagEventDTO where
  toJSON EditTagEventDTO {..} =
    object
      [ "eventType" .= "EditTagEvent"
      , "uuid" .= _editTagEventDTOUuid
      , "path" .= _editTagEventDTOPath
      , "tagUuid" .= _editTagEventDTOTagUuid
      , "name" .= _editTagEventDTOName
      , "description" .= _editTagEventDTODescription
      , "color" .= _editTagEventDTOColor
      ]

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON (Object o) = do
    _deleteTagEventDTOUuid <- o .: "uuid"
    _deleteTagEventDTOPath <- o .: "path"
    _deleteTagEventDTOTagUuid <- o .: "tagUuid"
    return DeleteTagEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteTagEventDTO where
  toJSON DeleteTagEventDTO {..} =
    object
      [ "eventType" .= "DeleteTagEvent"
      , "uuid" .= _deleteTagEventDTOUuid
      , "path" .= _deleteTagEventDTOPath
      , "tagUuid" .= _deleteTagEventDTOTagUuid
      ]

-- -------------------------
-- Integration -------------
-- -------------------------
instance FromJSON AddIntegrationEventDTO where
  parseJSON (Object o) = do
    _addIntegrationEventDTOUuid <- o .: "uuid"
    _addIntegrationEventDTOPath <- o .: "path"
    _addIntegrationEventDTOIntegrationUuid <- o .: "integrationUuid"
    _addIntegrationEventDTOIId <- o .: "id"
    _addIntegrationEventDTOName <- o .: "name"
    _addIntegrationEventDTOProps <- o .: "props"
    _addIntegrationEventDTOLogo <- o .: "logo"
    _addIntegrationEventDTORequestMethod <- o .: "requestMethod"
    _addIntegrationEventDTORequestUrl <- o .: "requestUrl"
    _addIntegrationEventDTORequestHeaders <- o .: "requestHeaders"
    _addIntegrationEventDTORequestBody <- o .: "requestBody"
    _addIntegrationEventDTOResponseListField <- o .: "responseListField"
    _addIntegrationEventDTOResponseIdField <- o .: "responseIdField"
    _addIntegrationEventDTOResponseNameField <- o .: "responseNameField"
    _addIntegrationEventDTOItemUrl <- o .: "itemUrl"
    return AddIntegrationEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddIntegrationEventDTO where
  toJSON AddIntegrationEventDTO {..} =
    object
      [ "eventType" .= "AddIntegrationEvent"
      , "uuid" .= _addIntegrationEventDTOUuid
      , "path" .= _addIntegrationEventDTOPath
      , "integrationUuid" .= _addIntegrationEventDTOIntegrationUuid
      , "id" .= _addIntegrationEventDTOIId
      , "name" .= _addIntegrationEventDTOName
      , "props" .= _addIntegrationEventDTOProps
      , "logo" .= _addIntegrationEventDTOLogo
      , "requestMethod" .= _addIntegrationEventDTORequestMethod
      , "requestUrl" .= _addIntegrationEventDTORequestUrl
      , "requestHeaders" .= _addIntegrationEventDTORequestHeaders
      , "requestBody" .= _addIntegrationEventDTORequestBody
      , "responseListField" .= _addIntegrationEventDTOResponseListField
      , "responseIdField" .= _addIntegrationEventDTOResponseIdField
      , "responseNameField" .= _addIntegrationEventDTOResponseNameField
      , "itemUrl" .= _addIntegrationEventDTOItemUrl
      ]

-- --------------------------------------------
instance FromJSON EditIntegrationEventDTO where
  parseJSON (Object o) = do
    _editIntegrationEventDTOUuid <- o .: "uuid"
    _editIntegrationEventDTOPath <- o .: "path"
    _editIntegrationEventDTOIntegrationUuid <- o .: "integrationUuid"
    _editIntegrationEventDTOIId <- o .: "id"
    _editIntegrationEventDTOName <- o .: "name"
    _editIntegrationEventDTOProps <- o .: "props"
    _editIntegrationEventDTOLogo <- o .: "logo"
    _editIntegrationEventDTORequestMethod <- o .: "requestMethod"
    _editIntegrationEventDTORequestUrl <- o .: "requestUrl"
    _editIntegrationEventDTORequestHeaders <- o .: "requestHeaders"
    _editIntegrationEventDTORequestBody <- o .: "requestBody"
    _editIntegrationEventDTOResponseListField <- o .: "responseListField"
    _editIntegrationEventDTOResponseIdField <- o .: "responseIdField"
    _editIntegrationEventDTOResponseNameField <- o .: "responseNameField"
    _editIntegrationEventDTOItemUrl <- o .: "itemUrl"
    return EditIntegrationEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditIntegrationEventDTO where
  toJSON EditIntegrationEventDTO {..} =
    object
      [ "eventType" .= "EditIntegrationEvent"
      , "uuid" .= _editIntegrationEventDTOUuid
      , "path" .= _editIntegrationEventDTOPath
      , "integrationUuid" .= _editIntegrationEventDTOIntegrationUuid
      , "id" .= _editIntegrationEventDTOIId
      , "name" .= _editIntegrationEventDTOName
      , "props" .= _editIntegrationEventDTOProps
      , "logo" .= _editIntegrationEventDTOLogo
      , "requestMethod" .= _editIntegrationEventDTORequestMethod
      , "requestUrl" .= _editIntegrationEventDTORequestUrl
      , "requestHeaders" .= _editIntegrationEventDTORequestHeaders
      , "requestBody" .= _editIntegrationEventDTORequestBody
      , "responseListField" .= _editIntegrationEventDTOResponseListField
      , "responseIdField" .= _editIntegrationEventDTOResponseIdField
      , "responseNameField" .= _editIntegrationEventDTOResponseNameField
      , "itemUrl" .= _editIntegrationEventDTOItemUrl
      ]

-- --------------------------------------------
instance FromJSON DeleteIntegrationEventDTO where
  parseJSON (Object o) = do
    _deleteIntegrationEventDTOUuid <- o .: "uuid"
    _deleteIntegrationEventDTOPath <- o .: "path"
    _deleteIntegrationEventDTOIntegrationUuid <- o .: "integrationUuid"
    return DeleteIntegrationEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteIntegrationEventDTO where
  toJSON DeleteIntegrationEventDTO {..} =
    object
      [ "eventType" .= "DeleteIntegrationEvent"
      , "uuid" .= _deleteIntegrationEventDTOUuid
      , "path" .= _deleteIntegrationEventDTOPath
      , "integrationUuid" .= _deleteIntegrationEventDTOIntegrationUuid
      ]

-- EventField DTO
data EventFieldDTO a
  = NothingChangedDTO
  | ChangedValueDTO a
  deriving (Show, Eq)

instance Functor EventFieldDTO where
  fmap f (ChangedValueDTO a) = ChangedValueDTO (f a)
  fmap _ NothingChangedDTO = NothingChangedDTO

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
  } deriving (Show, Eq)

type EventPathDTO = [EventPathItemDTO]

instance FromJSON EventPathItemDTO where
  parseJSON (Object o) = do
    _eventPathItemDTOPType <- o .: "type"
    _eventPathItemDTOUuid <- o .: "uuid"
    return EventPathItemDTO {..}
  parseJSON _ = mzero

instance ToJSON EventPathItemDTO where
  toJSON EventPathItemDTO {..} = object ["type" .= _eventPathItemDTOPType, "uuid" .= _eventPathItemDTOUuid]

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

-- Common
serializeQuestionValueType :: QuestionValueType -> String
serializeQuestionValueType questionType =
  case questionType of
    StringQuestionValueType -> "StringValue"
    NumberQuestionValueType -> "NumberValue"
    DateQuestionValueType -> "DateValue"
    TextQuestionValueType -> "TextValue"

deserializeQuestionValueType :: String -> Maybe QuestionValueType
deserializeQuestionValueType "StringValue" = Just StringQuestionValueType
deserializeQuestionValueType "NumberValue" = Just NumberQuestionValueType
deserializeQuestionValueType "DateValue" = Just DateQuestionValueType
deserializeQuestionValueType "TextValue" = Just TextQuestionValueType
deserializeQuestionValueType _ = Nothing

deserializeEventFieldQuestionValueType :: EventFieldDTO String -> EventFieldDTO QuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "StringValue") = ChangedValueDTO StringQuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "NumberValue") = ChangedValueDTO NumberQuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "DateValue") = ChangedValueDTO DateQuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "TextValue") = ChangedValueDTO TextQuestionValueType
deserializeEventFieldQuestionValueType _ = NothingChangedDTO
