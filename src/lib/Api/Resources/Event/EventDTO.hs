module Api.Resources.Event.EventDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics

import Common.Types
import Common.Uuid

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
  { _akmdtoUuid :: UUID
  , _akmdtoKmUuid :: UUID
  , _akmdtoName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO = EditKnowledgeModelEventDTO
  { _ekmdtoUuid :: UUID
  , _ekmdtoKmUuid :: UUID
  , _ekmdtoName :: Maybe String
  , _ekmdtoChapterIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Chapter -----------------
-- -------------------------
data AddChapterEventDTO = AddChapterEventDTO
  { _achdtoUuid :: UUID
  , _achdtoKmUuid :: UUID
  , _achdtoChapterUuid :: UUID
  , _achdtoTitle :: String
  , _achdtoText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEventDTO = EditChapterEventDTO
  { _echdtoUuid :: UUID
  , _echdtoKmUuid :: UUID
  , _echdtoChapterUuid :: UUID
  , _echdtoTitle :: Maybe String
  , _echdtoText :: Maybe String
  , _echdtoQuestionIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _dchdtoUuid :: UUID
  , _dchdtoKmUuid :: UUID
  , _dchdtoChapterUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Question ----------------
-- -------------------------
data AddQuestionEventDTO = AddQuestionEventDTO
  { _aqdtoUuid :: UUID
  , _aqdtoKmUuid :: UUID
  , _aqdtoChapterUuid :: UUID
  , _aqdtoQuestionUuid :: UUID
  , _aqdtoShortQuestionUuid :: Maybe String
  , _aqdtoType :: String
  , _aqdtoTitle :: String
  , _aqdtoText :: String
  } deriving (Show, Eq, Generic)

data EditQuestionEventDTO = EditQuestionEventDTO
  { _eqdtoUuid :: UUID
  , _eqdtoKmUuid :: UUID
  , _eqdtoChapterUuid :: UUID
  , _eqdtoQuestionUuid :: UUID
  , _eqdtoShortQuestionUuid :: Maybe (Maybe String)
  , _eqdtoType :: Maybe String
  , _eqdtoTitle :: Maybe String
  , _eqdtoText :: Maybe String
  , _eqdtoAnswerIds :: Maybe [UUID]
  , _eqdtoExpertIds :: Maybe [UUID]
  , _eqdtoReferenceIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteQuestionEventDTO = DeleteQuestionEventDTO
  { _dqdtoUuid :: UUID
  , _dqdtoKmUuid :: UUID
  , _dqdtoChapterUuid :: UUID
  , _dqdtoQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Answer ------------------
-- -------------------------
data AddAnswerEventDTO = AddAnswerEventDTO
  { _aansdtoUuid :: UUID
  , _aansdtoKmUuid :: UUID
  , _aansdtoChapterUuid :: UUID
  , _aansdtoQuestionUuid :: UUID
  , _aansdtoAnswerUuid :: UUID
  , _aansdtoLabel :: String
  , _aansdtoAdvice :: Maybe String
  } deriving (Show, Eq, Generic)

data EditAnswerEventDTO = EditAnswerEventDTO
  { _eansdtoUuid :: UUID
  , _eansdtoKmUuid :: UUID
  , _eansdtoChapterUuid :: UUID
  , _eansdtoQuestionUuid :: UUID
  , _eansdtoAnswerUuid :: UUID
  , _eansdtoLabel :: Maybe String
  , _eansdtoAdvice :: Maybe (Maybe String)
  , _eansdtoFollowUpIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO = DeleteAnswerEventDTO
  { _dansdtoUuid :: UUID
  , _dansdtoKmUuid :: UUID
  , _dansdtoChapterUuid :: UUID
  , _dansdtoQuestionUuid :: UUID
  , _dansdtoAnswerUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Expert ------------------
-- -------------------------
data AddExpertEventDTO = AddExpertEventDTO
  { _aexpdtoUuid :: UUID
  , _aexpdtoKmUuid :: UUID
  , _aexpdtoChapterUuid :: UUID
  , _aexpdtoQuestionUuid :: UUID
  , _aexpdtoExpertUuid :: UUID
  , _aexpdtoName :: String
  , _aexpdtoEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEventDTO = EditExpertEventDTO
  { _eexpdtoUuid :: UUID
  , _eexpdtoKmUuid :: UUID
  , _eexpdtoChapterUuid :: UUID
  , _eexpdtoQuestionUuid :: UUID
  , _eexpdtoExpertUuid :: UUID
  , _eexpdtoName :: Maybe String
  , _eexpdtoEmail :: Maybe String
  } deriving (Show, Eq, Generic)

data DeleteExpertEventDTO = DeleteExpertEventDTO
  { _dexpdtoUuid :: UUID
  , _dexpdtoKmUuid :: UUID
  , _dexpdtoChapterUuid :: UUID
  , _dexpdtoQuestionUuid :: UUID
  , _dexpdtoExpertUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Reference ---------------
-- -------------------------
data AddReferenceEventDTO = AddReferenceEventDTO
  { _arefdtoUuid :: UUID
  , _arefdtoKmUuid :: UUID
  , _arefdtoChapterUuid :: UUID
  , _arefdtoQuestionUuid :: UUID
  , _arefdtoReferenceUuid :: UUID
  , _arefdtoChapter :: String
  } deriving (Show, Eq, Generic)

data EditReferenceEventDTO = EditReferenceEventDTO
  { _erefdtoUuid :: UUID
  , _erefdtoKmUuid :: UUID
  , _erefdtoChapterUuid :: UUID
  , _erefdtoQuestionUuid :: UUID
  , _erefdtoReferenceUuid :: UUID
  , _erefdtoChapter :: Maybe String
  } deriving (Show, Eq, Generic)

data DeleteReferenceEventDTO = DeleteReferenceEventDTO
  { _drefdtoUuid :: UUID
  , _drefdtoKmUuid :: UUID
  , _drefdtoChapterUuid :: UUID
  , _drefdtoQuestionUuid :: UUID
  , _drefdtoReferenceUuid :: UUID
  } deriving (Show, Eq, Generic)

-- -------------------------
-- Follow up question ------
-- -------------------------
data AddFollowUpQuestionEventDTO = AddFollowUpQuestionEventDTO
  { _afuqdtoUuid :: UUID
  , _afuqdtoKmUuid :: UUID
  , _afuqdtoChapterUuid :: UUID
  , _afuqdtoAnswerUuid :: UUID
  , _afuqdtoQuestionUuid :: UUID
  , _afuqdtoShortQuestionUuid :: Maybe String
  , _afuqdtoType :: String
  , _afuqdtoTitle :: String
  , _afuqdtoText :: String
  } deriving (Show, Eq, Generic)

data EditFollowUpQuestionEventDTO = EditFollowUpQuestionEventDTO
  { _efuqdtoUuid :: UUID
  , _efuqdtoKmUuid :: UUID
  , _efuqdtoChapterUuid :: UUID
  , _efuqdtoAnswerUuid :: UUID
  , _efuqdtoQuestionUuid :: UUID
  , _efuqdtoShortQuestionUuid :: Maybe (Maybe String)
  , _efuqdtoType :: Maybe String
  , _efuqdtoTitle :: Maybe String
  , _efuqdtoText :: Maybe String
  , _efuqdtoAnswerIds :: Maybe [UUID]
  , _efuqdtoExpertIds :: Maybe [UUID]
  , _efuqdtoReferenceIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteFollowUpQuestionEventDTO = DeleteFollowUpQuestionEventDTO
  { _dfuqdtoUuid :: UUID
  , _dfuqdtoKmUuid :: UUID
  , _dfuqdtoChapterUuid :: UUID
  , _dfuqdtoAnswerUuid :: UUID
  , _dfuqdtoQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)

makeLenses ''AddKnowledgeModelEventDTO

makeLenses ''EditKnowledgeModelEventDTO

makeLenses ''AddChapterEventDTO

makeLenses ''EditChapterEventDTO

makeLenses ''DeleteChapterEventDTO

makeLenses ''AddQuestionEventDTO

makeLenses ''EditQuestionEventDTO

makeLenses ''DeleteQuestionEventDTO

makeLenses ''AddAnswerEventDTO

makeLenses ''EditAnswerEventDTO

makeLenses ''DeleteAnswerEventDTO

makeLenses ''AddExpertEventDTO

makeLenses ''EditExpertEventDTO

makeLenses ''DeleteExpertEventDTO

makeLenses ''AddReferenceEventDTO

makeLenses ''EditReferenceEventDTO

makeLenses ''DeleteReferenceEventDTO

makeLenses ''AddFollowUpQuestionEventDTO

makeLenses ''EditFollowUpQuestionEventDTO

makeLenses ''DeleteFollowUpQuestionEventDTO

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _akmdtoUuid <- o .: "uuid"
    _akmdtoKmUuid <- o .: "kmUuid"
    _akmdtoName <- o .: "name"
    return AddKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON AddKnowledgeModelEventDTO {..} =
    object
      ["eventType" .= "AddKnowledgeModelEvent", "uuid" .= _akmdtoUuid, "kmUuid" .= _akmdtoKmUuid, "name" .= _akmdtoName]

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _ekmdtoUuid <- o .: "uuid"
    _ekmdtoKmUuid <- o .: "kmUuid"
    _ekmdtoName <- o .: "name"
    _ekmdtoChapterIds <- o .: "chapterIds"
    return EditKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON EditKnowledgeModelEventDTO {..} =
    object
      [ "eventType" .= "EditKnowledgeModelEvent"
      , "uuid" .= _ekmdtoUuid
      , "kmUuid" .= _ekmdtoKmUuid
      , "name" .= _ekmdtoName
      , "chapterIds" .= _ekmdtoChapterIds
      ]

-------------------------
-- Chapter --------------
-------------------------
instance FromJSON AddChapterEventDTO where
  parseJSON (Object o) = do
    _achdtoUuid <- o .: "uuid"
    _achdtoKmUuid <- o .: "kmUuid"
    _achdtoChapterUuid <- o .: "chapterUuid"
    _achdtoTitle <- o .: "title"
    _achdtoText <- o .: "text"
    return AddChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddChapterEventDTO where
  toJSON AddChapterEventDTO {..} =
    object
      [ "eventType" .= "AddChapterEvent"
      , "uuid" .= _achdtoUuid
      , "kmUuid" .= _achdtoKmUuid
      , "chapterUuid" .= _achdtoChapterUuid
      , "title" .= _achdtoTitle
      , "text" .= _achdtoText
      ]

instance FromJSON EditChapterEventDTO where
  parseJSON (Object o) = do
    _echdtoUuid <- o .: "uuid"
    _echdtoKmUuid <- o .: "kmUuid"
    _echdtoChapterUuid <- o .: "chapterUuid"
    _echdtoTitle <- o .: "title"
    _echdtoText <- o .: "text"
    _echdtoQuestionIds <- o .: "questionIds"
    return EditChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditChapterEventDTO where
  toJSON EditChapterEventDTO {..} =
    object
      [ "eventType" .= "EditChapterEvent"
      , "uuid" .= _echdtoUuid
      , "kmUuid" .= _echdtoKmUuid
      , "chapterUuid" .= _echdtoChapterUuid
      , "title" .= _echdtoTitle
      , "text" .= _echdtoText
      , "questionIds" .= _echdtoQuestionIds
      ]

instance FromJSON DeleteChapterEventDTO where
  parseJSON (Object o) = do
    _dchdtoUuid <- o .: "uuid"
    _dchdtoKmUuid <- o .: "kmUuid"
    _dchdtoChapterUuid <- o .: "chapterUuid"
    return DeleteChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteChapterEventDTO where
  toJSON DeleteChapterEventDTO {..} =
    object
      [ "eventType" .= "DeleteChapterEvent"
      , "uuid" .= _dchdtoUuid
      , "kmUuid" .= _dchdtoKmUuid
      , "chapterUuid" .= _dchdtoChapterUuid
      ]

-- -------------------------
-- Question ----------------
-- -------------------------
instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    _aqdtoUuid <- o .: "uuid"
    _aqdtoKmUuid <- o .: "kmUuid"
    _aqdtoChapterUuid <- o .: "chapterUuid"
    _aqdtoQuestionUuid <- o .: "questionUuid"
    _aqdtoShortQuestionUuid <- o .: "shortQuestionUuid"
    _aqdtoType <- o .: "type"
    _aqdtoTitle <- o .: "title"
    _aqdtoText <- o .: "text"
    return AddQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddQuestionEventDTO where
  toJSON AddQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddQuestionEvent"
      , "uuid" .= _aqdtoUuid
      , "kmUuid" .= _aqdtoKmUuid
      , "chapterUuid" .= _aqdtoChapterUuid
      , "questionUuid" .= _aqdtoQuestionUuid
      , "shortQuestionUuid" .= _aqdtoShortQuestionUuid
      , "type" .= _aqdtoType
      , "title" .= _aqdtoTitle
      , "text" .= _aqdtoText
      ]

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    _eqdtoUuid <- o .: "uuid"
    _eqdtoKmUuid <- o .: "kmUuid"
    _eqdtoChapterUuid <- o .: "chapterUuid"
    _eqdtoQuestionUuid <- o .: "questionUuid"
    _eqdtoShortQuestionUuid <- o .: "shortQuestionUuid"
    _eqdtoType <- o .: "type"
    _eqdtoTitle <- o .: "title"
    _eqdtoText <- o .: "text"
    _eqdtoAnswerIds <- o .: "answerIds"
    _eqdtoExpertIds <- o .: "expertIds"
    _eqdtoReferenceIds <- o .: "referenceIds"
    return EditQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditQuestionEventDTO where
  toJSON EditQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditQuestionEvent"
      , "uuid" .= _eqdtoUuid
      , "kmUuid" .= _eqdtoKmUuid
      , "chapterUuid" .= _eqdtoChapterUuid
      , "questionUuid" .= _eqdtoQuestionUuid
      , "shortQuestionUuid" .= _eqdtoShortQuestionUuid
      , "type" .= _eqdtoType
      , "title" .= _eqdtoTitle
      , "text" .= _eqdtoText
      , "answerIds" .= _eqdtoAnswerIds
      , "expertIds" .= _eqdtoExpertIds
      , "referenceIds" .= _eqdtoReferenceIds
      ]

instance FromJSON DeleteQuestionEventDTO where
  parseJSON (Object o) = do
    _dqdtoUuid <- o .: "uuid"
    _dqdtoKmUuid <- o .: "kmUuid"
    _dqdtoChapterUuid <- o .: "chapterUuid"
    _dqdtoQuestionUuid <- o .: "questionUuid"
    return DeleteQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteQuestionEventDTO where
  toJSON DeleteQuestionEventDTO {..} =
    object
      [ "eventType" .= "DeleteQuestionEvent"
      , "uuid" .= _dqdtoUuid
      , "kmUuid" .= _dqdtoKmUuid
      , "chapterUuid" .= _dqdtoChapterUuid
      , "questionUuid" .= _dqdtoQuestionUuid
      ]

-- -------------------------
-- Answer ------------------
-- -------------------------
instance FromJSON AddAnswerEventDTO where
  parseJSON (Object o) = do
    _aansdtoUuid <- o .: "uuid"
    _aansdtoKmUuid <- o .: "kmUuid"
    _aansdtoChapterUuid <- o .: "chapterUuid"
    _aansdtoQuestionUuid <- o .: "questionUuid"
    _aansdtoAnswerUuid <- o .: "answerUuid"
    _aansdtoLabel <- o .: "label"
    _aansdtoAdvice <- o .: "advice"
    return AddAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddAnswerEventDTO where
  toJSON AddAnswerEventDTO {..} =
    object
      [ "eventType" .= "AddAnswerEvent"
      , "uuid" .= _aansdtoUuid
      , "kmUuid" .= _aansdtoKmUuid
      , "chapterUuid" .= _aansdtoChapterUuid
      , "questionUuid" .= _aansdtoQuestionUuid
      , "answerUuid" .= _aansdtoAnswerUuid
      , "label" .= _aansdtoLabel
      , "advice" .= _aansdtoAdvice
      ]

instance FromJSON EditAnswerEventDTO where
  parseJSON (Object o) = do
    _eansdtoUuid <- o .: "uuid"
    _eansdtoKmUuid <- o .: "kmUuid"
    _eansdtoChapterUuid <- o .: "chapterUuid"
    _eansdtoQuestionUuid <- o .: "questionUuid"
    _eansdtoAnswerUuid <- o .: "answerUuid"
    _eansdtoLabel <- o .: "label"
    _eansdtoAdvice <- o .: "advice"
    _eansdtoFollowUpIds <- o .: "followUpIds"
    return EditAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditAnswerEventDTO where
  toJSON EditAnswerEventDTO {..} =
    object
      [ "eventType" .= "EditAnswerEvent"
      , "uuid" .= _eansdtoUuid
      , "kmUuid" .= _eansdtoKmUuid
      , "chapterUuid" .= _eansdtoChapterUuid
      , "questionUuid" .= _eansdtoQuestionUuid
      , "answerUuid" .= _eansdtoAnswerUuid
      , "label" .= _eansdtoLabel
      , "advice" .= _eansdtoAdvice
      , "followUpIds" .= _eansdtoFollowUpIds
      ]

instance FromJSON DeleteAnswerEventDTO where
  parseJSON (Object o) = do
    _dansdtoUuid <- o .: "uuid"
    _dansdtoKmUuid <- o .: "kmUuid"
    _dansdtoChapterUuid <- o .: "chapterUuid"
    _dansdtoQuestionUuid <- o .: "questionUuid"
    _dansdtoAnswerUuid <- o .: "answerUuid"
    return DeleteAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteAnswerEventDTO where
  toJSON DeleteAnswerEventDTO {..} =
    object
      [ "eventType" .= "DeleteAnswerEvent"
      , "uuid" .= _dansdtoUuid
      , "kmUuid" .= _dansdtoKmUuid
      , "chapterUuid" .= _dansdtoChapterUuid
      , "questionUuid" .= _dansdtoQuestionUuid
      , "answerUuid" .= _dansdtoAnswerUuid
      ]

-- -------------------------
-- Expert ------------------
-- -------------------------
instance FromJSON AddExpertEventDTO where
  parseJSON (Object o) = do
    _aexpdtoUuid <- o .: "uuid"
    _aexpdtoKmUuid <- o .: "kmUuid"
    _aexpdtoChapterUuid <- o .: "chapterUuid"
    _aexpdtoQuestionUuid <- o .: "questionUuid"
    _aexpdtoExpertUuid <- o .: "expertUuid"
    _aexpdtoName <- o .: "name"
    _aexpdtoEmail <- o .: "email"
    return AddExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddExpertEventDTO where
  toJSON AddExpertEventDTO {..} =
    object
      [ "eventType" .= "AddExpertEvent"
      , "uuid" .= _aexpdtoUuid
      , "kmUuid" .= _aexpdtoKmUuid
      , "chapterUuid" .= _aexpdtoChapterUuid
      , "questionUuid" .= _aexpdtoQuestionUuid
      , "expertUuid" .= _aexpdtoExpertUuid
      , "name" .= _aexpdtoName
      , "email" .= _aexpdtoEmail
      ]

instance FromJSON EditExpertEventDTO where
  parseJSON (Object o) = do
    _eexpdtoUuid <- o .: "uuid"
    _eexpdtoKmUuid <- o .: "kmUuid"
    _eexpdtoChapterUuid <- o .: "chapterUuid"
    _eexpdtoQuestionUuid <- o .: "questionUuid"
    _eexpdtoExpertUuid <- o .: "expertUuid"
    _eexpdtoName <- o .: "name"
    _eexpdtoEmail <- o .: "email"
    return EditExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditExpertEventDTO where
  toJSON EditExpertEventDTO {..} =
    object
      [ "eventType" .= "EditExpertEvent"
      , "uuid" .= _eexpdtoUuid
      , "kmUuid" .= _eexpdtoKmUuid
      , "chapterUuid" .= _eexpdtoChapterUuid
      , "questionUuid" .= _eexpdtoQuestionUuid
      , "expertUuid" .= _eexpdtoExpertUuid
      , "name" .= _eexpdtoName
      , "email" .= _eexpdtoEmail
      ]

instance FromJSON DeleteExpertEventDTO where
  parseJSON (Object o) = do
    _dexpdtoUuid <- o .: "uuid"
    _dexpdtoKmUuid <- o .: "kmUuid"
    _dexpdtoChapterUuid <- o .: "chapterUuid"
    _dexpdtoQuestionUuid <- o .: "questionUuid"
    _dexpdtoExpertUuid <- o .: "expertUuid"
    return DeleteExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteExpertEventDTO where
  toJSON DeleteExpertEventDTO {..} =
    object
      [ "eventType" .= "DeleteExpertEvent"
      , "uuid" .= _dexpdtoUuid
      , "kmUuid" .= _dexpdtoKmUuid
      , "chapterUuid" .= _dexpdtoChapterUuid
      , "questionUuid" .= _dexpdtoQuestionUuid
      , "expertUuid" .= _dexpdtoExpertUuid
      ]

-- -------------------------
-- Reference ---------------
-- -------------------------      
instance FromJSON AddReferenceEventDTO where
  parseJSON (Object o) = do
    _arefdtoUuid <- o .: "uuid"
    _arefdtoKmUuid <- o .: "kmUuid"
    _arefdtoChapterUuid <- o .: "chapterUuid"
    _arefdtoQuestionUuid <- o .: "questionUuid"
    _arefdtoReferenceUuid <- o .: "referenceUuid"
    _arefdtoChapter <- o .: "chapter"
    return AddReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddReferenceEventDTO where
  toJSON AddReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "uuid" .= _arefdtoUuid
      , "kmUuid" .= _arefdtoKmUuid
      , "chapterUuid" .= _arefdtoChapterUuid
      , "questionUuid" .= _arefdtoQuestionUuid
      , "referenceUuid" .= _arefdtoReferenceUuid
      , "chapter" .= _arefdtoChapter
      ]

instance FromJSON EditReferenceEventDTO where
  parseJSON (Object o) = do
    _erefdtoUuid <- o .: "uuid"
    _erefdtoKmUuid <- o .: "kmUuid"
    _erefdtoChapterUuid <- o .: "chapterUuid"
    _erefdtoQuestionUuid <- o .: "questionUuid"
    _erefdtoReferenceUuid <- o .: "referenceUuid"
    _erefdtoChapter <- o .: "chapter"
    return EditReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditReferenceEventDTO where
  toJSON EditReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "uuid" .= _erefdtoUuid
      , "kmUuid" .= _erefdtoKmUuid
      , "chapterUuid" .= _erefdtoChapterUuid
      , "questionUuid" .= _erefdtoQuestionUuid
      , "referenceUuid" .= _erefdtoReferenceUuid
      , "chapter" .= _erefdtoChapter
      ]

instance FromJSON DeleteReferenceEventDTO where
  parseJSON (Object o) = do
    _drefdtoUuid <- o .: "uuid"
    _drefdtoKmUuid <- o .: "kmUuid"
    _drefdtoChapterUuid <- o .: "chapterUuid"
    _drefdtoQuestionUuid <- o .: "questionUuid"
    _drefdtoReferenceUuid <- o .: "referenceUuid"
    return DeleteReferenceEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteReferenceEventDTO where
  toJSON DeleteReferenceEventDTO {..} =
    object
      [ "eventType" .= "DeleteReferenceEvent"
      , "uuid" .= _drefdtoUuid
      , "kmUuid" .= _drefdtoKmUuid
      , "chapterUuid" .= _drefdtoChapterUuid
      , "questionUuid" .= _drefdtoQuestionUuid
      , "referenceUuid" .= _drefdtoReferenceUuid
      ]

-- -------------------------
-- Follow up question ------
-- -------------------------
instance FromJSON AddFollowUpQuestionEventDTO where
  parseJSON (Object o) = do
    _afuqdtoUuid <- o .: "uuid"
    _afuqdtoKmUuid <- o .: "kmUuid"
    _afuqdtoChapterUuid <- o .: "chapterUuid"
    _afuqdtoAnswerUuid <- o .: "answerUuid"
    _afuqdtoQuestionUuid <- o .: "questionUuid"
    _afuqdtoShortQuestionUuid <- o .: "shortQuestionUuid"
    _afuqdtoType <- o .: "type"
    _afuqdtoTitle <- o .: "title"
    _afuqdtoText <- o .: "text"
    return AddFollowUpQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddFollowUpQuestionEventDTO where
  toJSON AddFollowUpQuestionEventDTO {..} =
    object
      [ "eventType" .= "AddFollowUpQuestionEvent"
      , "uuid" .= _afuqdtoUuid
      , "kmUuid" .= _afuqdtoKmUuid
      , "chapterUuid" .= _afuqdtoChapterUuid
      , "answerUuid" .= _afuqdtoAnswerUuid
      , "questionUuid" .= _afuqdtoQuestionUuid
      , "shortQuestionUuid" .= _afuqdtoShortQuestionUuid
      , "type" .= _afuqdtoType
      , "title" .= _afuqdtoTitle
      , "text" .= _afuqdtoText
      ]

instance FromJSON EditFollowUpQuestionEventDTO where
  parseJSON (Object o) = do
    _efuqdtoUuid <- o .: "uuid"
    _efuqdtoKmUuid <- o .: "kmUuid"
    _efuqdtoChapterUuid <- o .: "chapterUuid"
    _efuqdtoAnswerUuid <- o .: "answerUuid"
    _efuqdtoQuestionUuid <- o .: "questionUuid"
    _efuqdtoShortQuestionUuid <- o .: "shortQuestionUuid"
    _efuqdtoType <- o .: "type"
    _efuqdtoTitle <- o .: "title"
    _efuqdtoText <- o .: "text"
    _efuqdtoAnswerIds <- o .: "answerIds"
    _efuqdtoExpertIds <- o .: "expertIds"
    _efuqdtoReferenceIds <- o .: "referenceIds"
    return EditFollowUpQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditFollowUpQuestionEventDTO where
  toJSON EditFollowUpQuestionEventDTO {..} =
    object
      [ "eventType" .= "EditFollowUpQuestionEvent"
      , "uuid" .= _efuqdtoUuid
      , "kmUuid" .= _efuqdtoKmUuid
      , "chapterUuid" .= _efuqdtoChapterUuid
      , "answerUuid" .= _efuqdtoAnswerUuid
      , "questionUuid" .= _efuqdtoQuestionUuid
      , "shortQuestionUuid" .= _efuqdtoShortQuestionUuid
      , "type" .= _efuqdtoType
      , "title" .= _efuqdtoTitle
      , "text" .= _efuqdtoText
      , "answerIds" .= _efuqdtoAnswerIds
      , "expertIds" .= _efuqdtoExpertIds
      , "referenceIds" .= _efuqdtoReferenceIds
      ]

instance FromJSON DeleteFollowUpQuestionEventDTO where
  parseJSON (Object o) = do
    _dfuqdtoUuid <- o .: "uuid"
    _dfuqdtoKmUuid <- o .: "kmUuid"
    _dfuqdtoChapterUuid <- o .: "chapterUuid"
    _dfuqdtoAnswerUuid <- o .: "answerUuid"
    _dfuqdtoQuestionUuid <- o .: "questionUuid"
    return DeleteFollowUpQuestionEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteFollowUpQuestionEventDTO where
  toJSON DeleteFollowUpQuestionEventDTO {..} =
    object
      [ "eventType" .= "DeleteFollowUpQuestionEvent"
      , "uuid" .= _dfuqdtoUuid
      , "kmUuid" .= _dfuqdtoKmUuid
      , "chapterUuid" .= _dfuqdtoChapterUuid
      , "answerUuid" .= _dfuqdtoAnswerUuid
      , "questionUuid" .= _dfuqdtoQuestionUuid
      ]
