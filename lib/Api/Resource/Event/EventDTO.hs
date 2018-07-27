module Api.Resource.Event.EventDTO where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Common
import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.KnowledgeModel.KnowledgeModel

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
  , _editKnowledgeModelEventDTOChapterIds :: EventFieldDTO [U.UUID]
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
  , _editChapterEventDTOQuestionIds :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _deleteChapterEventDTOUuid :: U.UUID
  , _deleteChapterEventDTOPath :: EventPathDTO
  , _deleteChapterEventDTOChapterUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Question ----------------
-- -------------------------
data AddQuestionEventDTO = AddQuestionEventDTO
  { _addQuestionEventDTOUuid :: U.UUID
  , _addQuestionEventDTOPath :: EventPathDTO
  , _addQuestionEventDTOQuestionUuid :: U.UUID
  , _addQuestionEventDTOQType :: QuestionType
  , _addQuestionEventDTOTitle :: String
  , _addQuestionEventDTOText :: String
  , _addQuestionEventDTORequiredLevel :: Maybe Int
  , _addQuestionEventDTOAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlainDTO
  } deriving (Show, Eq, Generic)

data EditQuestionEventDTO = EditQuestionEventDTO
  { _editQuestionEventDTOUuid :: U.UUID
  , _editQuestionEventDTOPath :: EventPathDTO
  , _editQuestionEventDTOQuestionUuid :: U.UUID
  , _editQuestionEventDTOQType :: EventFieldDTO QuestionType
  , _editQuestionEventDTOTitle :: EventFieldDTO String
  , _editQuestionEventDTOText :: EventFieldDTO String
  , _editQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
  , _editQuestionEventDTOAnswerItemTemplatePlainWithIds :: EventFieldDTO (Maybe AnswerItemTemplatePlainWithIdsDTO)
  , _editQuestionEventDTOAnswerIds :: EventFieldDTO (Maybe [U.UUID])
  , _editQuestionEventDTOExpertIds :: EventFieldDTO [U.UUID]
  , _editQuestionEventDTOReferenceIds :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

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
  , _editAnswerEventDTOFollowUpIds :: EventFieldDTO [U.UUID]
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

data EditReferenceEventDTO
  = EditResourcePageReferenceEventDTO' EditResourcePageReferenceEventDTO
  | EditURLReferenceEventDTO' EditURLReferenceEventDTO
  | EditCrossReferenceEventDTO' EditCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data DeleteReferenceEventDTO
  = DeleteResourcePageReferenceEventDTO' DeleteResourcePageReferenceEventDTO
  | DeleteURLReferenceEventDTO' DeleteURLReferenceEventDTO
  | DeleteCrossReferenceEventDTO' DeleteCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data AddResourcePageReferenceEventDTO = AddResourcePageReferenceEventDTO
  { _addResourcePageReferenceEventDTOUuid :: U.UUID
  , _addResourcePageReferenceEventDTOPath :: EventPathDTO
  , _addResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  , _addResourcePageReferenceEventDTOShortUuid :: String
  } deriving (Show, Eq, Generic)

data EditResourcePageReferenceEventDTO = EditResourcePageReferenceEventDTO
  { _editResourcePageReferenceEventDTOUuid :: U.UUID
  , _editResourcePageReferenceEventDTOPath :: EventPathDTO
  , _editResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  , _editResourcePageReferenceEventDTOShortUuid :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteResourcePageReferenceEventDTO = DeleteResourcePageReferenceEventDTO
  { _deleteResourcePageReferenceEventDTOUuid :: U.UUID
  , _deleteResourcePageReferenceEventDTOPath :: EventPathDTO
  , _deleteResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data AddURLReferenceEventDTO = AddURLReferenceEventDTO
  { _addURLReferenceEventDTOUuid :: U.UUID
  , _addURLReferenceEventDTOPath :: EventPathDTO
  , _addURLReferenceEventDTOReferenceUuid :: U.UUID
  , _addURLReferenceEventDTOUrl :: String
  , _addURLReferenceEventDTOAnchor :: String
  } deriving (Show, Eq, Generic)

data EditURLReferenceEventDTO = EditURLReferenceEventDTO
  { _editURLReferenceEventDTOUuid :: U.UUID
  , _editURLReferenceEventDTOPath :: EventPathDTO
  , _editURLReferenceEventDTOReferenceUuid :: U.UUID
  , _editURLReferenceEventDTOUrl :: EventFieldDTO String
  , _editURLReferenceEventDTOAnchor :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteURLReferenceEventDTO = DeleteURLReferenceEventDTO
  { _deleteURLReferenceEventDTOUuid :: U.UUID
  , _deleteURLReferenceEventDTOPath :: EventPathDTO
  , _deleteURLReferenceEventDTOReferenceUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data AddCrossReferenceEventDTO = AddCrossReferenceEventDTO
  { _addCrossReferenceEventDTOUuid :: U.UUID
  , _addCrossReferenceEventDTOPath :: EventPathDTO
  , _addCrossReferenceEventDTOReferenceUuid :: U.UUID
  , _addCrossReferenceEventDTOTargetUuid :: U.UUID
  , _addCrossReferenceEventDTODescription :: String
  } deriving (Show, Eq, Generic)

data EditCrossReferenceEventDTO = EditCrossReferenceEventDTO
  { _editCrossReferenceEventDTOUuid :: U.UUID
  , _editCrossReferenceEventDTOPath :: EventPathDTO
  , _editCrossReferenceEventDTOReferenceUuid :: U.UUID
  , _editCrossReferenceEventDTOTargetUuid :: EventFieldDTO U.UUID
  , _editCrossReferenceEventDTODescription :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteCrossReferenceEventDTO = DeleteCrossReferenceEventDTO
  { _deleteCrossReferenceEventDTOUuid :: U.UUID
  , _deleteCrossReferenceEventDTOPath :: EventPathDTO
  , _deleteCrossReferenceEventDTOReferenceUuid :: U.UUID
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
    _editKnowledgeModelEventDTOChapterIds <- o .: "chapterIds"
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
      , "chapterIds" .= _editKnowledgeModelEventDTOChapterIds
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
    _editChapterEventDTOQuestionIds <- o .: "questionIds"
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
      , "questionIds" .= _editChapterEventDTOQuestionIds
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
instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    _addQuestionEventDTOUuid <- o .: "uuid"
    _addQuestionEventDTOPath <- o .: "path"
    _addQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addQuestionEventDTOTitle <- o .: "title"
    _addQuestionEventDTOText <- o .: "text"
    _addQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _addQuestionEventDTOAnswerItemTemplatePlain <- o .: "answerItemTemplate"
    questionType <- o .: "type"
    case deserializeQuestionType questionType of
      (Just _addQuestionEventDTOQType) -> return AddQuestionEventDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance ToJSON AddQuestionEventDTO where
  toJSON AddQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "uuid" .= _addQuestionEventDTOUuid
      , "path" .= _addQuestionEventDTOPath
      , "questionUuid" .= _addQuestionEventDTOQuestionUuid
      , "type" .= serializeQuestionType _addQuestionEventDTOQType
      , "title" .= _addQuestionEventDTOTitle
      , "text" .= _addQuestionEventDTOText
      , "requiredLevel" .= _addQuestionEventDTORequiredLevel
      , "answerItemTemplate" .= _addQuestionEventDTOAnswerItemTemplatePlain
      ]

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    _editQuestionEventDTOUuid <- o .: "uuid"
    _editQuestionEventDTOPath <- o .: "path"
    _editQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editQuestionEventDTOTitle <- o .: "title"
    _editQuestionEventDTOText <- o .: "text"
    _editQuestionEventDTORequiredLevel <- o .: "requiredLevel"
    _editQuestionEventDTOAnswerItemTemplatePlainWithIds <- o .: "answerItemTemplate"
    _editQuestionEventDTOAnswerIds <- o .: "answerIds"
    _editQuestionEventDTOExpertIds <- o .: "expertIds"
    _editQuestionEventDTOReferenceIds <- o .: "referenceIds"
    questionType <- o .: "type"
    case deserializeEventFieldQuestionType <$> questionType of
      (Just _editQuestionEventDTOQType) -> return EditQuestionEventDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance ToJSON EditQuestionEventDTO where
  toJSON EditQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "uuid" .= _editQuestionEventDTOUuid
      , "path" .= _editQuestionEventDTOPath
      , "questionUuid" .= _editQuestionEventDTOQuestionUuid
      , "type" .= (serializeQuestionType <$> _editQuestionEventDTOQType)
      , "title" .= _editQuestionEventDTOTitle
      , "text" .= _editQuestionEventDTOText
      , "requiredLevel" .= _editQuestionEventDTORequiredLevel
      , "answerItemTemplate" .= _editQuestionEventDTOAnswerItemTemplatePlainWithIds
      , "answerIds" .= _editQuestionEventDTOAnswerIds
      , "expertIds" .= _editQuestionEventDTOExpertIds
      , "referenceIds" .= _editQuestionEventDTOReferenceIds
      ]

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
    _editAnswerEventDTOFollowUpIds <- o .: "followUpIds"
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
      , "followUpIds" .= _editAnswerEventDTOFollowUpIds
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
instance ToJSON DeleteReferenceEventDTO where
  toJSON (DeleteResourcePageReferenceEventDTO' event) = toJSON event
  toJSON (DeleteURLReferenceEventDTO' event) = toJSON event
  toJSON (DeleteCrossReferenceEventDTO' event) = toJSON event

instance FromJSON DeleteReferenceEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (DeleteResourcePageReferenceEventDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (DeleteURLReferenceEventDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (DeleteCrossReferenceEventDTO' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
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
instance FromJSON DeleteResourcePageReferenceEventDTO where
  parseJSON (Object o) = do
    _deleteResourcePageReferenceEventDTOUuid <- o .: "uuid"
    _deleteResourcePageReferenceEventDTOPath <- o .: "path"
    _deleteResourcePageReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    return DeleteResourcePageReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteResourcePageReferenceEventDTO where
  toJSON DeleteResourcePageReferenceEventDTO {..} =
    object
      [ "eventType" .= "DeleteReferenceEvent"
      , "referenceType" .= "ResourcePageReference"
      , "uuid" .= _deleteResourcePageReferenceEventDTOUuid
      , "path" .= _deleteResourcePageReferenceEventDTOPath
      , "referenceUuid" .= _deleteResourcePageReferenceEventDTOReferenceUuid
      ]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON AddURLReferenceEventDTO where
  parseJSON (Object o) = do
    _addURLReferenceEventDTOUuid <- o .: "uuid"
    _addURLReferenceEventDTOPath <- o .: "path"
    _addURLReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _addURLReferenceEventDTOUrl <- o .: "url"
    _addURLReferenceEventDTOAnchor <- o .: "anchor"
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
      , "anchor" .= _addURLReferenceEventDTOAnchor
      ]

-- --------------------------------------------
instance FromJSON EditURLReferenceEventDTO where
  parseJSON (Object o) = do
    _editURLReferenceEventDTOUuid <- o .: "uuid"
    _editURLReferenceEventDTOPath <- o .: "path"
    _editURLReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _editURLReferenceEventDTOUrl <- o .: "url"
    _editURLReferenceEventDTOAnchor <- o .: "anchor"
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
      , "anchor" .= _editURLReferenceEventDTOAnchor
      ]

-- --------------------------------------------
instance FromJSON DeleteURLReferenceEventDTO where
  parseJSON (Object o) = do
    _deleteURLReferenceEventDTOUuid <- o .: "uuid"
    _deleteURLReferenceEventDTOPath <- o .: "path"
    _deleteURLReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    return DeleteURLReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteURLReferenceEventDTO where
  toJSON DeleteURLReferenceEventDTO {..} =
    object
      [ "eventType" .= "DeleteReferenceEvent"
      , "referenceType" .= "URLReference"
      , "uuid" .= _deleteURLReferenceEventDTOUuid
      , "path" .= _deleteURLReferenceEventDTOPath
      , "referenceUuid" .= _deleteURLReferenceEventDTOReferenceUuid
      ]

-- --------------------------------------------
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
instance FromJSON DeleteCrossReferenceEventDTO where
  parseJSON (Object o) = do
    _deleteCrossReferenceEventDTOUuid <- o .: "uuid"
    _deleteCrossReferenceEventDTOPath <- o .: "path"
    _deleteCrossReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    return DeleteCrossReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteCrossReferenceEventDTO where
  toJSON DeleteCrossReferenceEventDTO {..} =
    object
      [ "eventType" .= "DeleteReferenceEvent"
      , "referenceType" .= "CrossReference"
      , "uuid" .= _deleteCrossReferenceEventDTOUuid
      , "path" .= _deleteCrossReferenceEventDTOPath
      , "referenceUuid" .= _deleteCrossReferenceEventDTOReferenceUuid
      ]
