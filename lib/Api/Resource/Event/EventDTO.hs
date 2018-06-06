module Api.Resource.Event.EventDTO where

import Control.Monad
import Data.Aeson
import Data.UUID
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
  { _addKnowledgeModelEventDTOUuid :: UUID
  , _addKnowledgeModelEventDTOPath :: EventPathDTO
  , _addKnowledgeModelEventDTOKmUuid :: UUID
  , _addKnowledgeModelEventDTOName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO = EditKnowledgeModelEventDTO
  { _editKnowledgeModelEventDTOUuid :: UUID
  , _editKnowledgeModelEventDTOPath :: EventPathDTO
  , _editKnowledgeModelEventDTOKmUuid :: UUID
  , _editKnowledgeModelEventDTOName :: EventFieldDTO String
  , _editKnowledgeModelEventDTOChapterIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Chapter -----------------
-- -------------------------
data AddChapterEventDTO = AddChapterEventDTO
  { _addChapterEventDTOUuid :: UUID
  , _addChapterEventDTOPath :: EventPathDTO
  , _addChapterEventDTOChapterUuid :: UUID
  , _addChapterEventDTOTitle :: String
  , _addChapterEventDTOText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEventDTO = EditChapterEventDTO
  { _editChapterEventDTOUuid :: UUID
  , _editChapterEventDTOPath :: EventPathDTO
  , _editChapterEventDTOChapterUuid :: UUID
  , _editChapterEventDTOTitle :: EventFieldDTO String
  , _editChapterEventDTOText :: EventFieldDTO String
  , _editChapterEventDTOQuestionIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _deleteChapterEventDTOUuid :: UUID
  , _deleteChapterEventDTOPath :: EventPathDTO
  , _deleteChapterEventDTOChapterUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Question ----------------
-- -------------------------
data AddQuestionEventDTO = AddQuestionEventDTO
  { _addQuestionEventDTOUuid :: UUID
  , _addQuestionEventDTOPath :: EventPathDTO
  , _addQuestionEventDTOQuestionUuid :: UUID
  , _addQuestionEventDTOShortQuestionUuid :: Maybe String
  , _addQuestionEventDTOQType :: QuestionType
  , _addQuestionEventDTOTitle :: String
  , _addQuestionEventDTOText :: String
  , _addQuestionEventDTOAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlainDTO
  } deriving (Show, Eq, Generic)

data EditQuestionEventDTO = EditQuestionEventDTO
  { _editQuestionEventDTOUuid :: UUID
  , _editQuestionEventDTOPath :: EventPathDTO
  , _editQuestionEventDTOQuestionUuid :: UUID
  , _editQuestionEventDTOShortQuestionUuid :: EventFieldDTO (Maybe String)
  , _editQuestionEventDTOQType :: EventFieldDTO QuestionType
  , _editQuestionEventDTOTitle :: EventFieldDTO String
  , _editQuestionEventDTOText :: EventFieldDTO String
  , _editQuestionEventDTOAnswerItemTemplatePlainWithIds :: EventFieldDTO (Maybe AnswerItemTemplatePlainWithIdsDTO)
  , _editQuestionEventDTOAnswerIds :: EventFieldDTO (Maybe [UUID])
  , _editQuestionEventDTOExpertIds :: EventFieldDTO [UUID]
  , _editQuestionEventDTOReferenceIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteQuestionEventDTO = DeleteQuestionEventDTO
  { _deleteQuestionEventDTOUuid :: UUID
  , _deleteQuestionEventDTOPath :: EventPathDTO
  , _deleteQuestionEventDTOQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Answer ------------------
-- -------------------------
data AddAnswerEventDTO = AddAnswerEventDTO
  { _addAnswerEventDTOUuid :: UUID
  , _addAnswerEventDTOPath :: EventPathDTO
  , _addAnswerEventDTOAnswerUuid :: UUID
  , _addAnswerEventDTOLabel :: String
  , _addAnswerEventDTOAdvice :: Maybe String
  } deriving (Show, Eq, Generic)

data EditAnswerEventDTO = EditAnswerEventDTO
  { _editAnswerEventDTOUuid :: UUID
  , _editAnswerEventDTOPath :: EventPathDTO
  , _editAnswerEventDTOAnswerUuid :: UUID
  , _editAnswerEventDTOLabel :: EventFieldDTO String
  , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
  , _editAnswerEventDTOFollowUpIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO = DeleteAnswerEventDTO
  { _deleteAnswerEventDTOUuid :: UUID
  , _deleteAnswerEventDTOPath :: EventPathDTO
  , _deleteAnswerEventDTOAnswerUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Expert ------------------
-- -------------------------
data AddExpertEventDTO = AddExpertEventDTO
  { _addExpertEventDTOUuid :: UUID
  , _addExpertEventDTOPath :: EventPathDTO
  , _addExpertEventDTOExpertUuid :: UUID
  , _addExpertEventDTOName :: String
  , _addExpertEventDTOEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEventDTO = EditExpertEventDTO
  { _editExpertEventDTOUuid :: UUID
  , _editExpertEventDTOPath :: EventPathDTO
  , _editExpertEventDTOExpertUuid :: UUID
  , _editExpertEventDTOName :: EventFieldDTO String
  , _editExpertEventDTOEmail :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteExpertEventDTO = DeleteExpertEventDTO
  { _deleteExpertEventDTOUuid :: UUID
  , _deleteExpertEventDTOPath :: EventPathDTO
  , _deleteExpertEventDTOExpertUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Reference ---------------
-- -------------------------
data AddReferenceEventDTO = AddReferenceEventDTO
  { _addReferenceEventDTOUuid :: UUID
  , _addReferenceEventDTOPath :: EventPathDTO
  , _addReferenceEventDTOReferenceUuid :: UUID
  , _addReferenceEventDTOChapter :: String
  } deriving (Show, Eq, Generic)

data EditReferenceEventDTO = EditReferenceEventDTO
  { _editReferenceEventDTOUuid :: UUID
  , _editReferenceEventDTOPath :: EventPathDTO
  , _editReferenceEventDTOReferenceUuid :: UUID
  , _editReferenceEventDTOChapter :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteReferenceEventDTO = DeleteReferenceEventDTO
  { _deleteReferenceEventDTOUuid :: UUID
  , _deleteReferenceEventDTOPath :: EventPathDTO
  , _deleteReferenceEventDTOReferenceUuid :: UUID
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
    _addQuestionEventDTOShortQuestionUuid <- o .: "shortQuestionUuid"
    _addQuestionEventDTOTitle <- o .: "title"
    _addQuestionEventDTOText <- o .: "text"
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
      , "shortQuestionUuid" .= _addQuestionEventDTOShortQuestionUuid
      , "type" .= serializeQuestionType _addQuestionEventDTOQType
      , "title" .= _addQuestionEventDTOTitle
      , "text" .= _addQuestionEventDTOText
      , "answerItemTemplate" .= _addQuestionEventDTOAnswerItemTemplatePlain
      ]

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    _editQuestionEventDTOUuid <- o .: "uuid"
    _editQuestionEventDTOPath <- o .: "path"
    _editQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editQuestionEventDTOShortQuestionUuid <- o .: "shortQuestionUuid"
    _editQuestionEventDTOTitle <- o .: "title"
    _editQuestionEventDTOText <- o .: "text"
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
      , "shortQuestionUuid" .= _editQuestionEventDTOShortQuestionUuid
      , "type" .= (serializeQuestionType <$> _editQuestionEventDTOQType)
      , "title" .= _editQuestionEventDTOTitle
      , "text" .= _editQuestionEventDTOText
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
      ]

instance FromJSON EditAnswerEventDTO where
  parseJSON (Object o) = do
    _editAnswerEventDTOUuid <- o .: "uuid"
    _editAnswerEventDTOPath <- o .: "path"
    _editAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    _editAnswerEventDTOLabel <- o .: "label"
    _editAnswerEventDTOAdvice <- o .: "advice"
    _editAnswerEventDTOFollowUpIds <- o .: "followUpIds"
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
instance FromJSON AddReferenceEventDTO where
  parseJSON (Object o) = do
    _addReferenceEventDTOUuid <- o .: "uuid"
    _addReferenceEventDTOPath <- o .: "path"
    _addReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _addReferenceEventDTOChapter <- o .: "chapter"
    return AddReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddReferenceEventDTO where
  toJSON AddReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "uuid" .= _addReferenceEventDTOUuid
      , "path" .= _addReferenceEventDTOPath
      , "referenceUuid" .= _addReferenceEventDTOReferenceUuid
      , "chapter" .= _addReferenceEventDTOChapter
      ]

instance FromJSON EditReferenceEventDTO where
  parseJSON (Object o) = do
    _editReferenceEventDTOUuid <- o .: "uuid"
    _editReferenceEventDTOPath <- o .: "path"
    _editReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _editReferenceEventDTOChapter <- o .: "chapter"
    return EditReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditReferenceEventDTO where
  toJSON EditReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "uuid" .= _editReferenceEventDTOUuid
      , "path" .= _editReferenceEventDTOPath
      , "referenceUuid" .= _editReferenceEventDTOReferenceUuid
      , "chapter" .= _editReferenceEventDTOChapter
      ]

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
