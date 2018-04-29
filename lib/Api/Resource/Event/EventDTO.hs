module Api.Resource.Event.EventDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics

import Api.Resource.Common
import Api.Resource.Event.EventFieldDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Types
import Common.Uuid
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
  | AddAnswerItemTemplateQuestionEventDTO' AddAnswerItemTemplateQuestionEventDTO
  | EditAnswerItemTemplateQuestionEventDTO' EditAnswerItemTemplateQuestionEventDTO
  | DeleteAnswerItemTemplateQuestionEventDTO' DeleteAnswerItemTemplateQuestionEventDTO
  | AddExpertEventDTO' AddExpertEventDTO
  | EditExpertEventDTO' EditExpertEventDTO
  | DeleteExpertEventDTO' DeleteExpertEventDTO
  | AddReferenceEventDTO' AddReferenceEventDTO
  | EditReferenceEventDTO' EditReferenceEventDTO
  | DeleteReferenceEventDTO' DeleteReferenceEventDTO
  | AddFollowUpQuestionEventDTO' AddFollowUpQuestionEventDTO
  | EditFollowUpQuestionEventDTO' EditFollowUpQuestionEventDTO
  | DeleteFollowUpQuestionEventDTO' DeleteFollowUpQuestionEventDTO
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
  toJSON (AddAnswerItemTemplateQuestionEventDTO' event) = toJSON event
  toJSON (EditAnswerItemTemplateQuestionEventDTO' event) = toJSON event
  toJSON (DeleteAnswerItemTemplateQuestionEventDTO' event) = toJSON event
  toJSON (AddExpertEventDTO' event) = toJSON event
  toJSON (EditExpertEventDTO' event) = toJSON event
  toJSON (DeleteExpertEventDTO' event) = toJSON event
  toJSON (AddReferenceEventDTO' event) = toJSON event
  toJSON (EditReferenceEventDTO' event) = toJSON event
  toJSON (DeleteReferenceEventDTO' event) = toJSON event
  toJSON (AddFollowUpQuestionEventDTO' event) = toJSON event
  toJSON (EditFollowUpQuestionEventDTO' event) = toJSON event
  toJSON (DeleteFollowUpQuestionEventDTO' event) = toJSON event

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
      "AddAnswerItemTemplateQuestionEvent" ->
        parseJSON (Object o) >>= \event -> return (AddAnswerItemTemplateQuestionEventDTO' event)
      "EditAnswerItemTemplateQuestionEvent" ->
        parseJSON (Object o) >>= \event -> return (EditAnswerItemTemplateQuestionEventDTO' event)
      "DeleteAnswerItemTemplateQuestionEvent" ->
        parseJSON (Object o) >>= \event -> return (DeleteAnswerItemTemplateQuestionEventDTO' event)
      "AddExpertEvent" -> parseJSON (Object o) >>= \event -> return (AddExpertEventDTO' event)
      "EditExpertEvent" -> parseJSON (Object o) >>= \event -> return (EditExpertEventDTO' event)
      "DeleteExpertEvent" -> parseJSON (Object o) >>= \event -> return (DeleteExpertEventDTO' event)
      "AddReferenceEvent" -> parseJSON (Object o) >>= \event -> return (AddReferenceEventDTO' event)
      "EditReferenceEvent" -> parseJSON (Object o) >>= \event -> return (EditReferenceEventDTO' event)
      "DeleteReferenceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteReferenceEventDTO' event)
      "AddFollowUpQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddFollowUpQuestionEventDTO' event)
      "EditFollowUpQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditFollowUpQuestionEventDTO' event)
      "DeleteFollowUpQuestionEvent" -> parseJSON (Object o) >>= \event -> return (DeleteFollowUpQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported eventType"
  parseJSON _ = mzero

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
data AddKnowledgeModelEventDTO = AddKnowledgeModelEventDTO
  { _addKnowledgeModelEventDTOUuid :: UUID
  , _addKnowledgeModelEventDTOKmUuid :: UUID
  , _addKnowledgeModelEventDTOName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO = EditKnowledgeModelEventDTO
  { _editKnowledgeModelEventDTOUuid :: UUID
  , _editKnowledgeModelEventDTOKmUuid :: UUID
  , _editKnowledgeModelEventDTOName :: EventFieldDTO String
  , _editKnowledgeModelEventDTOChapterIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Chapter -----------------
-- -------------------------
data AddChapterEventDTO = AddChapterEventDTO
  { _addChapterEventDTOUuid :: UUID
  , _addChapterEventDTOKmUuid :: UUID
  , _addChapterEventDTOChapterUuid :: UUID
  , _addChapterEventDTOTitle :: String
  , _addChapterEventDTOText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEventDTO = EditChapterEventDTO
  { _editChapterEventDTOUuid :: UUID
  , _editChapterEventDTOKmUuid :: UUID
  , _editChapterEventDTOChapterUuid :: UUID
  , _editChapterEventDTOTitle :: EventFieldDTO String
  , _editChapterEventDTOText :: EventFieldDTO String
  , _editChapterEventDTOQuestionIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _deleteChapterEventDTOUuid :: UUID
  , _deleteChapterEventDTOKmUuid :: UUID
  , _deleteChapterEventDTOChapterUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Question ----------------
-- -------------------------
data AddQuestionEventDTO = AddQuestionEventDTO
  { _addQuestionEventDTOUuid :: UUID
  , _addQuestionEventDTOKmUuid :: UUID
  , _addQuestionEventDTOChapterUuid :: UUID
  , _addQuestionEventDTOQuestionUuid :: UUID
  , _addQuestionEventDTOShortQuestionUuid :: Maybe String
  , _addQuestionEventDTOQType :: QuestionType
  , _addQuestionEventDTOTitle :: String
  , _addQuestionEventDTOText :: String
  , _addQuestionEventDTOAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlainDTO
  } deriving (Show, Eq, Generic)

data EditQuestionEventDTO = EditQuestionEventDTO
  { _editQuestionEventDTOUuid :: UUID
  , _editQuestionEventDTOKmUuid :: UUID
  , _editQuestionEventDTOChapterUuid :: UUID
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
  , _deleteQuestionEventDTOKmUuid :: UUID
  , _deleteQuestionEventDTOChapterUuid :: UUID
  , _deleteQuestionEventDTOQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Answer ------------------
-- -------------------------
data AddAnswerEventDTO = AddAnswerEventDTO
  { _addAnswerEventDTOUuid :: UUID
  , _addAnswerEventDTOKmUuid :: UUID
  , _addAnswerEventDTOChapterUuid :: UUID
  , _addAnswerEventDTOQuestionUuid :: UUID
  , _addAnswerEventDTOAnswerUuid :: UUID
  , _addAnswerEventDTOLabel :: String
  , _addAnswerEventDTOAdvice :: Maybe String
  } deriving (Show, Eq, Generic)

data EditAnswerEventDTO = EditAnswerEventDTO
  { _editAnswerEventDTOUuid :: UUID
  , _editAnswerEventDTOKmUuid :: UUID
  , _editAnswerEventDTOChapterUuid :: UUID
  , _editAnswerEventDTOQuestionUuid :: UUID
  , _editAnswerEventDTOAnswerUuid :: UUID
  , _editAnswerEventDTOLabel :: EventFieldDTO String
  , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
  , _editAnswerEventDTOFollowUpIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO = DeleteAnswerEventDTO
  { _deleteAnswerEventDTOUuid :: UUID
  , _deleteAnswerEventDTOKmUuid :: UUID
  , _deleteAnswerEventDTOChapterUuid :: UUID
  , _deleteAnswerEventDTOQuestionUuid :: UUID
  , _deleteAnswerEventDTOAnswerUuid :: UUID
  } deriving (Show, Eq, Generic)

-- ---------------------------------
-- Answer Item Template Question ---
-- ---------------------------------
data AddAnswerItemTemplateQuestionEventDTO = AddAnswerItemTemplateQuestionEventDTO
  { _addAnswerItemTemplateQuestionEventDTOUuid :: UUID
  , _addAnswerItemTemplateQuestionEventDTOKmUuid :: UUID
  , _addAnswerItemTemplateQuestionEventDTOChapterUuid :: UUID
  , _addAnswerItemTemplateQuestionEventDTOParentQuestionUuid :: UUID
  , _addAnswerItemTemplateQuestionEventDTOQuestionUuid :: UUID
  , _addAnswerItemTemplateQuestionEventDTOShortQuestionUuid :: Maybe String
  , _addAnswerItemTemplateQuestionEventDTOQType :: QuestionType
  , _addAnswerItemTemplateQuestionEventDTOTitle :: String
  , _addAnswerItemTemplateQuestionEventDTOText :: String
  , _addAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlainDTO
  } deriving (Show, Eq, Generic)

data EditAnswerItemTemplateQuestionEventDTO = EditAnswerItemTemplateQuestionEventDTO
  { _editAnswerItemTemplateQuestionEventDTOUuid :: UUID
  , _editAnswerItemTemplateQuestionEventDTOKmUuid :: UUID
  , _editAnswerItemTemplateQuestionEventDTOChapterUuid :: UUID
  , _editAnswerItemTemplateQuestionEventDTOParentQuestionUuid :: UUID
  , _editAnswerItemTemplateQuestionEventDTOQuestionUuid :: UUID
  , _editAnswerItemTemplateQuestionEventDTOShortQuestionUuid :: EventFieldDTO (Maybe String)
  , _editAnswerItemTemplateQuestionEventDTOQType :: EventFieldDTO QuestionType
  , _editAnswerItemTemplateQuestionEventDTOTitle :: EventFieldDTO String
  , _editAnswerItemTemplateQuestionEventDTOText :: EventFieldDTO String
  , _editAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlainWithIds :: EventFieldDTO (Maybe AnswerItemTemplatePlainWithIdsDTO)
  , _editAnswerItemTemplateQuestionEventDTOAnswerIds :: EventFieldDTO (Maybe [UUID])
  , _editAnswerItemTemplateQuestionEventDTOExpertIds :: EventFieldDTO [UUID]
  , _editAnswerItemTemplateQuestionEventDTOReferenceIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerItemTemplateQuestionEventDTO = DeleteAnswerItemTemplateQuestionEventDTO
  { _deleteAnswerItemTemplateQuestionEventDTOUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventDTOKmUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventDTOParentQuestionUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventDTOChapterUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventDTOQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Expert ------------------
-- -------------------------
data AddExpertEventDTO = AddExpertEventDTO
  { _addExpertEventDTOUuid :: UUID
  , _addExpertEventDTOKmUuid :: UUID
  , _addExpertEventDTOChapterUuid :: UUID
  , _addExpertEventDTOQuestionUuid :: UUID
  , _addExpertEventDTOExpertUuid :: UUID
  , _addExpertEventDTOName :: String
  , _addExpertEventDTOEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEventDTO = EditExpertEventDTO
  { _editExpertEventDTOUuid :: UUID
  , _editExpertEventDTOKmUuid :: UUID
  , _editExpertEventDTOChapterUuid :: UUID
  , _editExpertEventDTOQuestionUuid :: UUID
  , _editExpertEventDTOExpertUuid :: UUID
  , _editExpertEventDTOName :: EventFieldDTO String
  , _editExpertEventDTOEmail :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteExpertEventDTO = DeleteExpertEventDTO
  { _deleteExpertEventDTOUuid :: UUID
  , _deleteExpertEventDTOKmUuid :: UUID
  , _deleteExpertEventDTOChapterUuid :: UUID
  , _deleteExpertEventDTOQuestionUuid :: UUID
  , _deleteExpertEventDTOExpertUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Reference ---------------
-- -------------------------
data AddReferenceEventDTO = AddReferenceEventDTO
  { _addReferenceEventDTOUuid :: UUID
  , _addReferenceEventDTOKmUuid :: UUID
  , _addReferenceEventDTOChapterUuid :: UUID
  , _addReferenceEventDTOQuestionUuid :: UUID
  , _addReferenceEventDTOReferenceUuid :: UUID
  , _addReferenceEventDTOChapter :: String
  } deriving (Show, Eq, Generic)

data EditReferenceEventDTO = EditReferenceEventDTO
  { _editReferenceEventDTOUuid :: UUID
  , _editReferenceEventDTOKmUuid :: UUID
  , _editReferenceEventDTOChapterUuid :: UUID
  , _editReferenceEventDTOQuestionUuid :: UUID
  , _editReferenceEventDTOReferenceUuid :: UUID
  , _editReferenceEventDTOChapter :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteReferenceEventDTO = DeleteReferenceEventDTO
  { _deleteReferenceEventDTOUuid :: UUID
  , _deleteReferenceEventDTOKmUuid :: UUID
  , _deleteReferenceEventDTOChapterUuid :: UUID
  , _deleteReferenceEventDTOQuestionUuid :: UUID
  , _deleteReferenceEventDTOReferenceUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Follow up question ------
-- -------------------------
data AddFollowUpQuestionEventDTO = AddFollowUpQuestionEventDTO
  { _addFollowUpQuestionEventDTOUuid :: UUID
  , _addFollowUpQuestionEventDTOKmUuid :: UUID
  , _addFollowUpQuestionEventDTOChapterUuid :: UUID
  , _addFollowUpQuestionEventDTOAnswerUuid :: UUID
  , _addFollowUpQuestionEventDTOQuestionUuid :: UUID
  , _addFollowUpQuestionEventDTOShortQuestionUuid :: Maybe String
  , _addFollowUpQuestionEventDTOQType :: QuestionType
  , _addFollowUpQuestionEventDTOTitle :: String
  , _addFollowUpQuestionEventDTOText :: String
  , _addFollowUpQuestionEventDTOAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlainDTO
  } deriving (Show, Eq, Generic)

data EditFollowUpQuestionEventDTO = EditFollowUpQuestionEventDTO
  { _editFollowUpQuestionEventDTOUuid :: UUID
  , _editFollowUpQuestionEventDTOKmUuid :: UUID
  , _editFollowUpQuestionEventDTOChapterUuid :: UUID
  , _editFollowUpQuestionEventDTOAnswerUuid :: UUID
  , _editFollowUpQuestionEventDTOQuestionUuid :: UUID
  , _editFollowUpQuestionEventDTOShortQuestionUuid :: EventFieldDTO (Maybe String)
  , _editFollowUpQuestionEventDTOQType :: EventFieldDTO QuestionType
  , _editFollowUpQuestionEventDTOTitle :: EventFieldDTO String
  , _editFollowUpQuestionEventDTOText :: EventFieldDTO String
  , _editFollowUpQuestionEventDTOAnswerItemTemplatePlainWithIds :: EventFieldDTO (Maybe AnswerItemTemplatePlainWithIdsDTO)
  , _editFollowUpQuestionEventDTOAnswerIds :: EventFieldDTO (Maybe [UUID])
  , _editFollowUpQuestionEventDTOExpertIds :: EventFieldDTO [UUID]
  , _editFollowUpQuestionEventDTOReferenceIds :: EventFieldDTO [UUID]
  } deriving (Show, Eq, Generic)

data DeleteFollowUpQuestionEventDTO = DeleteFollowUpQuestionEventDTO
  { _deleteFollowUpQuestionEventDTOUuid :: UUID
  , _deleteFollowUpQuestionEventDTOKmUuid :: UUID
  , _deleteFollowUpQuestionEventDTOChapterUuid :: UUID
  , _deleteFollowUpQuestionEventDTOAnswerUuid :: UUID
  , _deleteFollowUpQuestionEventDTOQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _addKnowledgeModelEventDTOUuid <- o .: "uuid"
    _addKnowledgeModelEventDTOKmUuid <- o .: "kmUuid"
    _addKnowledgeModelEventDTOName <- o .: "name"
    return AddKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON AddKnowledgeModelEventDTO {..} =
    object
      [ "eventType" .= "AddKnowledgeModelEvent"
      , "uuid" .= _addKnowledgeModelEventDTOUuid
      , "kmUuid" .= _addKnowledgeModelEventDTOKmUuid
      , "name" .= _addKnowledgeModelEventDTOName
      ]

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _editKnowledgeModelEventDTOUuid <- o .: "uuid"
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
    _addChapterEventDTOKmUuid <- o .: "kmUuid"
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
      , "kmUuid" .= _addChapterEventDTOKmUuid
      , "chapterUuid" .= _addChapterEventDTOChapterUuid
      , "title" .= _addChapterEventDTOTitle
      , "text" .= _addChapterEventDTOText
      ]

instance FromJSON EditChapterEventDTO where
  parseJSON (Object o) = do
    _editChapterEventDTOUuid <- o .: "uuid"
    _editChapterEventDTOKmUuid <- o .: "kmUuid"
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
      , "kmUuid" .= _editChapterEventDTOKmUuid
      , "chapterUuid" .= _editChapterEventDTOChapterUuid
      , "title" .= _editChapterEventDTOTitle
      , "text" .= _editChapterEventDTOText
      , "questionIds" .= _editChapterEventDTOQuestionIds
      ]

instance FromJSON DeleteChapterEventDTO where
  parseJSON (Object o) = do
    _deleteChapterEventDTOUuid <- o .: "uuid"
    _deleteChapterEventDTOKmUuid <- o .: "kmUuid"
    _deleteChapterEventDTOChapterUuid <- o .: "chapterUuid"
    return DeleteChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteChapterEventDTO where
  toJSON DeleteChapterEventDTO {..} =
    object
      [ "eventType" .= "DeleteChapterEvent"
      , "uuid" .= _deleteChapterEventDTOUuid
      , "kmUuid" .= _deleteChapterEventDTOKmUuid
      , "chapterUuid" .= _deleteChapterEventDTOChapterUuid
      ]

-- -------------------------
-- Question ----------------
-- -------------------------
instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    _addQuestionEventDTOUuid <- o .: "uuid"
    _addQuestionEventDTOKmUuid <- o .: "kmUuid"
    _addQuestionEventDTOChapterUuid <- o .: "chapterUuid"
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
      , "kmUuid" .= _addQuestionEventDTOKmUuid
      , "chapterUuid" .= _addQuestionEventDTOChapterUuid
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
    _editQuestionEventDTOKmUuid <- o .: "kmUuid"
    _editQuestionEventDTOChapterUuid <- o .: "chapterUuid"
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
      , "kmUuid" .= _editQuestionEventDTOKmUuid
      , "chapterUuid" .= _editQuestionEventDTOChapterUuid
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
    _deleteQuestionEventDTOKmUuid <- o .: "kmUuid"
    _deleteQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _deleteQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    return DeleteQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteQuestionEventDTO where
  toJSON DeleteQuestionEventDTO {..} =
    object
      [ "eventType" .= "DeleteQuestionEvent"
      , "uuid" .= _deleteQuestionEventDTOUuid
      , "kmUuid" .= _deleteQuestionEventDTOKmUuid
      , "chapterUuid" .= _deleteQuestionEventDTOChapterUuid
      , "questionUuid" .= _deleteQuestionEventDTOQuestionUuid
      ]

-- -------------------------
-- Answer ------------------
-- -------------------------
instance FromJSON AddAnswerEventDTO where
  parseJSON (Object o) = do
    _addAnswerEventDTOUuid <- o .: "uuid"
    _addAnswerEventDTOKmUuid <- o .: "kmUuid"
    _addAnswerEventDTOChapterUuid <- o .: "chapterUuid"
    _addAnswerEventDTOQuestionUuid <- o .: "questionUuid"
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
      , "kmUuid" .= _addAnswerEventDTOKmUuid
      , "chapterUuid" .= _addAnswerEventDTOChapterUuid
      , "questionUuid" .= _addAnswerEventDTOQuestionUuid
      , "answerUuid" .= _addAnswerEventDTOAnswerUuid
      , "label" .= _addAnswerEventDTOLabel
      , "advice" .= _addAnswerEventDTOAdvice
      ]

instance FromJSON EditAnswerEventDTO where
  parseJSON (Object o) = do
    _editAnswerEventDTOUuid <- o .: "uuid"
    _editAnswerEventDTOKmUuid <- o .: "kmUuid"
    _editAnswerEventDTOChapterUuid <- o .: "chapterUuid"
    _editAnswerEventDTOQuestionUuid <- o .: "questionUuid"
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
      , "kmUuid" .= _editAnswerEventDTOKmUuid
      , "chapterUuid" .= _editAnswerEventDTOChapterUuid
      , "questionUuid" .= _editAnswerEventDTOQuestionUuid
      , "answerUuid" .= _editAnswerEventDTOAnswerUuid
      , "label" .= _editAnswerEventDTOLabel
      , "advice" .= _editAnswerEventDTOAdvice
      , "followUpIds" .= _editAnswerEventDTOFollowUpIds
      ]

instance FromJSON DeleteAnswerEventDTO where
  parseJSON (Object o) = do
    _deleteAnswerEventDTOUuid <- o .: "uuid"
    _deleteAnswerEventDTOKmUuid <- o .: "kmUuid"
    _deleteAnswerEventDTOChapterUuid <- o .: "chapterUuid"
    _deleteAnswerEventDTOQuestionUuid <- o .: "questionUuid"
    _deleteAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    return DeleteAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteAnswerEventDTO where
  toJSON DeleteAnswerEventDTO {..} =
    object
      [ "eventType" .= "DeleteAnswerEvent"
      , "uuid" .= _deleteAnswerEventDTOUuid
      , "kmUuid" .= _deleteAnswerEventDTOKmUuid
      , "chapterUuid" .= _deleteAnswerEventDTOChapterUuid
      , "questionUuid" .= _deleteAnswerEventDTOQuestionUuid
      , "answerUuid" .= _deleteAnswerEventDTOAnswerUuid
      ]

-- ---------------------------------
-- Answer Item Template Question ---
-- ---------------------------------
instance FromJSON AddAnswerItemTemplateQuestionEventDTO where
  parseJSON (Object o) = do
    _addAnswerItemTemplateQuestionEventDTOUuid <- o .: "uuid"
    _addAnswerItemTemplateQuestionEventDTOKmUuid <- o .: "kmUuid"
    _addAnswerItemTemplateQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _addAnswerItemTemplateQuestionEventDTOParentQuestionUuid <- o .: "parentQuestionUuid"
    _addAnswerItemTemplateQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addAnswerItemTemplateQuestionEventDTOShortQuestionUuid <- o .: "shortQuestionUuid"
    _addAnswerItemTemplateQuestionEventDTOTitle <- o .: "title"
    _addAnswerItemTemplateQuestionEventDTOText <- o .: "text"
    _addAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlain <- o .: "answerItemTemplate"
    questionType <- o .: "type"
    case deserializeQuestionType questionType of
      (Just _addAnswerItemTemplateQuestionEventDTOQType) -> return AddAnswerItemTemplateQuestionEventDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance ToJSON AddAnswerItemTemplateQuestionEventDTO where
  toJSON AddAnswerItemTemplateQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "uuid" .= _addAnswerItemTemplateQuestionEventDTOUuid
      , "kmUuid" .= _addAnswerItemTemplateQuestionEventDTOKmUuid
      , "chapterUuid" .= _addAnswerItemTemplateQuestionEventDTOChapterUuid
      , "parentQuestionUuid" .= _addAnswerItemTemplateQuestionEventDTOParentQuestionUuid
      , "questionUuid" .= _addAnswerItemTemplateQuestionEventDTOQuestionUuid
      , "shortQuestionUuid" .= _addAnswerItemTemplateQuestionEventDTOShortQuestionUuid
      , "type" .= serializeQuestionType _addAnswerItemTemplateQuestionEventDTOQType
      , "title" .= _addAnswerItemTemplateQuestionEventDTOTitle
      , "text" .= _addAnswerItemTemplateQuestionEventDTOText
      , "answerItemTemplate" .= _addAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlain
      ]

instance FromJSON EditAnswerItemTemplateQuestionEventDTO where
  parseJSON (Object o) = do
    _editAnswerItemTemplateQuestionEventDTOUuid <- o .: "uuid"
    _editAnswerItemTemplateQuestionEventDTOKmUuid <- o .: "kmUuid"
    _editAnswerItemTemplateQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _editAnswerItemTemplateQuestionEventDTOParentQuestionUuid <- o .: "parentQuestionUuid"
    _editAnswerItemTemplateQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editAnswerItemTemplateQuestionEventDTOShortQuestionUuid <- o .: "shortQuestionUuid"
    _editAnswerItemTemplateQuestionEventDTOTitle <- o .: "title"
    _editAnswerItemTemplateQuestionEventDTOText <- o .: "text"
    _editAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlainWithIds <- o .: "answerItemTemplate"
    _editAnswerItemTemplateQuestionEventDTOAnswerIds <- o .: "answerIds"
    _editAnswerItemTemplateQuestionEventDTOExpertIds <- o .: "expertIds"
    _editAnswerItemTemplateQuestionEventDTOReferenceIds <- o .: "referenceIds"
    questionType <- o .: "type"
    case deserializeEventFieldQuestionType <$> questionType of
      (Just _editAnswerItemTemplateQuestionEventDTOQType) -> return EditAnswerItemTemplateQuestionEventDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance ToJSON EditAnswerItemTemplateQuestionEventDTO where
  toJSON EditAnswerItemTemplateQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "uuid" .= _editAnswerItemTemplateQuestionEventDTOUuid
      , "kmUuid" .= _editAnswerItemTemplateQuestionEventDTOKmUuid
      , "chapterUuid" .= _editAnswerItemTemplateQuestionEventDTOChapterUuid
      , "parentQuestionUuid" .= _editAnswerItemTemplateQuestionEventDTOParentQuestionUuid
      , "questionUuid" .= _editAnswerItemTemplateQuestionEventDTOQuestionUuid
      , "shortQuestionUuid" .= _editAnswerItemTemplateQuestionEventDTOShortQuestionUuid
      , "type" .= (serializeQuestionType <$> _editAnswerItemTemplateQuestionEventDTOQType)
      , "title" .= _editAnswerItemTemplateQuestionEventDTOTitle
      , "text" .= _editAnswerItemTemplateQuestionEventDTOText
      , "answerItemTemplate" .= _editAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlainWithIds
      , "answerIds" .= _editAnswerItemTemplateQuestionEventDTOAnswerIds
      , "expertIds" .= _editAnswerItemTemplateQuestionEventDTOExpertIds
      , "referenceIds" .= _editAnswerItemTemplateQuestionEventDTOReferenceIds
      ]

instance FromJSON DeleteAnswerItemTemplateQuestionEventDTO where
  parseJSON (Object o) = do
    _deleteAnswerItemTemplateQuestionEventDTOUuid <- o .: "uuid"
    _deleteAnswerItemTemplateQuestionEventDTOKmUuid <- o .: "kmUuid"
    _deleteAnswerItemTemplateQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _deleteAnswerItemTemplateQuestionEventDTOParentQuestionUuid <- o .: "parentQuestionUuid"
    _deleteAnswerItemTemplateQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    return DeleteAnswerItemTemplateQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteAnswerItemTemplateQuestionEventDTO where
  toJSON DeleteAnswerItemTemplateQuestionEventDTO {..} =
    object
      [ "eventType" .= "DeleteQuestionEvent"
      , "uuid" .= _deleteAnswerItemTemplateQuestionEventDTOUuid
      , "kmUuid" .= _deleteAnswerItemTemplateQuestionEventDTOKmUuid
      , "chapterUuid" .= _deleteAnswerItemTemplateQuestionEventDTOChapterUuid
      , "parentQuestionUuid" .= _deleteAnswerItemTemplateQuestionEventDTOParentQuestionUuid
      , "questionUuid" .= _deleteAnswerItemTemplateQuestionEventDTOQuestionUuid
      ]

-- -------------------------
-- Expert ------------------
-- -------------------------
instance FromJSON AddExpertEventDTO where
  parseJSON (Object o) = do
    _addExpertEventDTOUuid <- o .: "uuid"
    _addExpertEventDTOKmUuid <- o .: "kmUuid"
    _addExpertEventDTOChapterUuid <- o .: "chapterUuid"
    _addExpertEventDTOQuestionUuid <- o .: "questionUuid"
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
      , "kmUuid" .= _addExpertEventDTOKmUuid
      , "chapterUuid" .= _addExpertEventDTOChapterUuid
      , "questionUuid" .= _addExpertEventDTOQuestionUuid
      , "expertUuid" .= _addExpertEventDTOExpertUuid
      , "name" .= _addExpertEventDTOName
      , "email" .= _addExpertEventDTOEmail
      ]

instance FromJSON EditExpertEventDTO where
  parseJSON (Object o) = do
    _editExpertEventDTOUuid <- o .: "uuid"
    _editExpertEventDTOKmUuid <- o .: "kmUuid"
    _editExpertEventDTOChapterUuid <- o .: "chapterUuid"
    _editExpertEventDTOQuestionUuid <- o .: "questionUuid"
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
      , "kmUuid" .= _editExpertEventDTOKmUuid
      , "chapterUuid" .= _editExpertEventDTOChapterUuid
      , "questionUuid" .= _editExpertEventDTOQuestionUuid
      , "expertUuid" .= _editExpertEventDTOExpertUuid
      , "name" .= _editExpertEventDTOName
      , "email" .= _editExpertEventDTOEmail
      ]

instance FromJSON DeleteExpertEventDTO where
  parseJSON (Object o) = do
    _deleteExpertEventDTOUuid <- o .: "uuid"
    _deleteExpertEventDTOKmUuid <- o .: "kmUuid"
    _deleteExpertEventDTOChapterUuid <- o .: "chapterUuid"
    _deleteExpertEventDTOQuestionUuid <- o .: "questionUuid"
    _deleteExpertEventDTOExpertUuid <- o .: "expertUuid"
    return DeleteExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteExpertEventDTO where
  toJSON DeleteExpertEventDTO {..} =
    object
      [ "eventType" .= "DeleteExpertEvent"
      , "uuid" .= _deleteExpertEventDTOUuid
      , "kmUuid" .= _deleteExpertEventDTOKmUuid
      , "chapterUuid" .= _deleteExpertEventDTOChapterUuid
      , "questionUuid" .= _deleteExpertEventDTOQuestionUuid
      , "expertUuid" .= _deleteExpertEventDTOExpertUuid
      ]

-- -------------------------
-- Reference ---------------
-- -------------------------
instance FromJSON AddReferenceEventDTO where
  parseJSON (Object o) = do
    _addReferenceEventDTOUuid <- o .: "uuid"
    _addReferenceEventDTOKmUuid <- o .: "kmUuid"
    _addReferenceEventDTOChapterUuid <- o .: "chapterUuid"
    _addReferenceEventDTOQuestionUuid <- o .: "questionUuid"
    _addReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _addReferenceEventDTOChapter <- o .: "chapter"
    return AddReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddReferenceEventDTO where
  toJSON AddReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "uuid" .= _addReferenceEventDTOUuid
      , "kmUuid" .= _addReferenceEventDTOKmUuid
      , "chapterUuid" .= _addReferenceEventDTOChapterUuid
      , "questionUuid" .= _addReferenceEventDTOQuestionUuid
      , "referenceUuid" .= _addReferenceEventDTOReferenceUuid
      , "chapter" .= _addReferenceEventDTOChapter
      ]

instance FromJSON EditReferenceEventDTO where
  parseJSON (Object o) = do
    _editReferenceEventDTOUuid <- o .: "uuid"
    _editReferenceEventDTOKmUuid <- o .: "kmUuid"
    _editReferenceEventDTOChapterUuid <- o .: "chapterUuid"
    _editReferenceEventDTOQuestionUuid <- o .: "questionUuid"
    _editReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    _editReferenceEventDTOChapter <- o .: "chapter"
    return EditReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditReferenceEventDTO where
  toJSON EditReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "uuid" .= _editReferenceEventDTOUuid
      , "kmUuid" .= _editReferenceEventDTOKmUuid
      , "chapterUuid" .= _editReferenceEventDTOChapterUuid
      , "questionUuid" .= _editReferenceEventDTOQuestionUuid
      , "referenceUuid" .= _editReferenceEventDTOReferenceUuid
      , "chapter" .= _editReferenceEventDTOChapter
      ]

instance FromJSON DeleteReferenceEventDTO where
  parseJSON (Object o) = do
    _deleteReferenceEventDTOUuid <- o .: "uuid"
    _deleteReferenceEventDTOKmUuid <- o .: "kmUuid"
    _deleteReferenceEventDTOChapterUuid <- o .: "chapterUuid"
    _deleteReferenceEventDTOQuestionUuid <- o .: "questionUuid"
    _deleteReferenceEventDTOReferenceUuid <- o .: "referenceUuid"
    return DeleteReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteReferenceEventDTO where
  toJSON DeleteReferenceEventDTO {..} =
    object
      [ "eventType" .= "DeleteReferenceEvent"
      , "uuid" .= _deleteReferenceEventDTOUuid
      , "kmUuid" .= _deleteReferenceEventDTOKmUuid
      , "chapterUuid" .= _deleteReferenceEventDTOChapterUuid
      , "questionUuid" .= _deleteReferenceEventDTOQuestionUuid
      , "referenceUuid" .= _deleteReferenceEventDTOReferenceUuid
      ]

-- -------------------------
-- Follow up question ------
-- -------------------------
instance FromJSON AddFollowUpQuestionEventDTO where
  parseJSON (Object o) = do
    _addFollowUpQuestionEventDTOUuid <- o .: "uuid"
    _addFollowUpQuestionEventDTOKmUuid <- o .: "kmUuid"
    _addFollowUpQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _addFollowUpQuestionEventDTOAnswerUuid <- o .: "answerUuid"
    _addFollowUpQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _addFollowUpQuestionEventDTOShortQuestionUuid <- o .: "shortQuestionUuid"
    _addFollowUpQuestionEventDTOTitle <- o .: "title"
    _addFollowUpQuestionEventDTOText <- o .: "text"
    _addFollowUpQuestionEventDTOAnswerItemTemplatePlain <- o .: "answerItemTemplate"
    questionType <- o .: "type"
    case deserializeQuestionType questionType of
      (Just _addFollowUpQuestionEventDTOQType) -> return AddFollowUpQuestionEventDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance ToJSON AddFollowUpQuestionEventDTO where
  toJSON AddFollowUpQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddFollowUpQuestionEvent"
      , "uuid" .= _addFollowUpQuestionEventDTOUuid
      , "kmUuid" .= _addFollowUpQuestionEventDTOKmUuid
      , "chapterUuid" .= _addFollowUpQuestionEventDTOChapterUuid
      , "answerUuid" .= _addFollowUpQuestionEventDTOAnswerUuid
      , "questionUuid" .= _addFollowUpQuestionEventDTOQuestionUuid
      , "shortQuestionUuid" .= _addFollowUpQuestionEventDTOShortQuestionUuid
      , "type" .= serializeQuestionType _addFollowUpQuestionEventDTOQType
      , "title" .= _addFollowUpQuestionEventDTOTitle
      , "text" .= _addFollowUpQuestionEventDTOText
      , "answerItemTemplate" .= _addFollowUpQuestionEventDTOAnswerItemTemplatePlain
      ]

instance FromJSON EditFollowUpQuestionEventDTO where
  parseJSON (Object o) = do
    _editFollowUpQuestionEventDTOUuid <- o .: "uuid"
    _editFollowUpQuestionEventDTOKmUuid <- o .: "kmUuid"
    _editFollowUpQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _editFollowUpQuestionEventDTOAnswerUuid <- o .: "answerUuid"
    _editFollowUpQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    _editFollowUpQuestionEventDTOShortQuestionUuid <- o .: "shortQuestionUuid"
    _editFollowUpQuestionEventDTOTitle <- o .: "title"
    _editFollowUpQuestionEventDTOText <- o .: "text"
    _editFollowUpQuestionEventDTOAnswerItemTemplatePlainWithIds <- o .: "answerItemTemplate"
    _editFollowUpQuestionEventDTOAnswerIds <- o .: "answerIds"
    _editFollowUpQuestionEventDTOExpertIds <- o .: "expertIds"
    _editFollowUpQuestionEventDTOReferenceIds <- o .: "referenceIds"
    questionType <- o .: "type"
    case deserializeEventFieldQuestionType <$> questionType of
      (Just _editFollowUpQuestionEventDTOQType) -> return EditFollowUpQuestionEventDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance ToJSON EditFollowUpQuestionEventDTO where
  toJSON EditFollowUpQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditFollowUpQuestionEvent"
      , "uuid" .= _editFollowUpQuestionEventDTOUuid
      , "kmUuid" .= _editFollowUpQuestionEventDTOKmUuid
      , "chapterUuid" .= _editFollowUpQuestionEventDTOChapterUuid
      , "answerUuid" .= _editFollowUpQuestionEventDTOAnswerUuid
      , "questionUuid" .= _editFollowUpQuestionEventDTOQuestionUuid
      , "shortQuestionUuid" .= _editFollowUpQuestionEventDTOShortQuestionUuid
      , "type" .= (serializeQuestionType <$> _editFollowUpQuestionEventDTOQType)
      , "title" .= _editFollowUpQuestionEventDTOTitle
      , "text" .= _editFollowUpQuestionEventDTOText
      , "answerItemTemplate" .= _editFollowUpQuestionEventDTOAnswerItemTemplatePlainWithIds
      , "answerIds" .= _editFollowUpQuestionEventDTOAnswerIds
      , "expertIds" .= _editFollowUpQuestionEventDTOExpertIds
      , "referenceIds" .= _editFollowUpQuestionEventDTOReferenceIds
      ]

instance FromJSON DeleteFollowUpQuestionEventDTO where
  parseJSON (Object o) = do
    _deleteFollowUpQuestionEventDTOUuid <- o .: "uuid"
    _deleteFollowUpQuestionEventDTOKmUuid <- o .: "kmUuid"
    _deleteFollowUpQuestionEventDTOChapterUuid <- o .: "chapterUuid"
    _deleteFollowUpQuestionEventDTOAnswerUuid <- o .: "answerUuid"
    _deleteFollowUpQuestionEventDTOQuestionUuid <- o .: "questionUuid"
    return DeleteFollowUpQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteFollowUpQuestionEventDTO where
  toJSON DeleteFollowUpQuestionEventDTO {..} =
    object
      [ "eventType" .= "DeleteFollowUpQuestionEvent"
      , "uuid" .= _deleteFollowUpQuestionEventDTOUuid
      , "kmUuid" .= _deleteFollowUpQuestionEventDTOKmUuid
      , "chapterUuid" .= _deleteFollowUpQuestionEventDTOChapterUuid
      , "answerUuid" .= _deleteFollowUpQuestionEventDTOAnswerUuid
      , "questionUuid" .= _deleteFollowUpQuestionEventDTOQuestionUuid
      ]
