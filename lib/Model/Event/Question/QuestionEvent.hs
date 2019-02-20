module Model.Event.Question.QuestionEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath
import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent
  = AddOptionsQuestionEvent' AddOptionsQuestionEvent
  | AddListQuestionEvent' AddListQuestionEvent
  | AddValueQuestionEvent' AddValueQuestionEvent
  deriving (Show, Eq, Generic)

data EditQuestionEvent
  = EditOptionsQuestionEvent' EditOptionsQuestionEvent
  | EditListQuestionEvent' EditListQuestionEvent
  | EditValueQuestionEvent' EditValueQuestionEvent
  deriving (Show, Eq, Generic)

data DeleteQuestionEvent
  = DeleteOptionsQuestionEvent' DeleteOptionsQuestionEvent
  | DeleteListQuestionEvent' DeleteListQuestionEvent
  | DeleteValueQuestionEvent' DeleteValueQuestionEvent
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data AddOptionsQuestionEvent = AddOptionsQuestionEvent
  { _addOptionsQuestionEventUuid :: U.UUID
  , _addOptionsQuestionEventPath :: EventPath
  , _addOptionsQuestionEventQuestionUuid :: U.UUID
  , _addOptionsQuestionEventTitle :: String
  , _addOptionsQuestionEventText :: Maybe String
  , _addOptionsQuestionEventRequiredLevel :: Maybe Int
  , _addOptionsQuestionEventTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

data EditOptionsQuestionEvent = EditOptionsQuestionEvent
  { _editOptionsQuestionEventUuid :: U.UUID
  , _editOptionsQuestionEventPath :: EventPath
  , _editOptionsQuestionEventQuestionUuid :: U.UUID
  , _editOptionsQuestionEventTitle :: EventField String
  , _editOptionsQuestionEventText :: EventField (Maybe String)
  , _editOptionsQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editOptionsQuestionEventTagUuids :: EventField [U.UUID]
  , _editOptionsQuestionEventExpertUuids :: EventField [U.UUID]
  , _editOptionsQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editOptionsQuestionEventAnswerUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteOptionsQuestionEvent = DeleteOptionsQuestionEvent
  { _deleteOptionsQuestionEventUuid :: U.UUID
  , _deleteOptionsQuestionEventPath :: EventPath
  , _deleteOptionsQuestionEventQuestionUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data AddListQuestionEvent = AddListQuestionEvent
  { _addListQuestionEventUuid :: U.UUID
  , _addListQuestionEventPath :: EventPath
  , _addListQuestionEventQuestionUuid :: U.UUID
  , _addListQuestionEventTitle :: String
  , _addListQuestionEventText :: Maybe String
  , _addListQuestionEventRequiredLevel :: Maybe Int
  , _addListQuestionEventTagUuids :: [U.UUID]
  , _addListQuestionEventItemTemplateTitle :: String
  } deriving (Show, Eq, Generic)

data EditListQuestionEvent = EditListQuestionEvent
  { _editListQuestionEventUuid :: U.UUID
  , _editListQuestionEventPath :: EventPath
  , _editListQuestionEventQuestionUuid :: U.UUID
  , _editListQuestionEventTitle :: EventField String
  , _editListQuestionEventText :: EventField (Maybe String)
  , _editListQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editListQuestionEventTagUuids :: EventField [U.UUID]
  , _editListQuestionEventExpertUuids :: EventField [U.UUID]
  , _editListQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editListQuestionEventItemTemplateTitle :: EventField String
  , _editListQuestionEventItemTemplateQuestionUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteListQuestionEvent = DeleteListQuestionEvent
  { _deleteListQuestionEventUuid :: U.UUID
  , _deleteListQuestionEventPath :: EventPath
  , _deleteListQuestionEventQuestionUuid :: U.UUID
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data AddValueQuestionEvent = AddValueQuestionEvent
  { _addValueQuestionEventUuid :: U.UUID
  , _addValueQuestionEventPath :: EventPath
  , _addValueQuestionEventQuestionUuid :: U.UUID
  , _addValueQuestionEventTitle :: String
  , _addValueQuestionEventText :: Maybe String
  , _addValueQuestionEventRequiredLevel :: Maybe Int
  , _addValueQuestionEventTagUuids :: [U.UUID]
  , _addValueQuestionEventValueType :: QuestionValueType
  } deriving (Show, Eq, Generic)

data EditValueQuestionEvent = EditValueQuestionEvent
  { _editValueQuestionEventUuid :: U.UUID
  , _editValueQuestionEventPath :: EventPath
  , _editValueQuestionEventQuestionUuid :: U.UUID
  , _editValueQuestionEventTitle :: EventField String
  , _editValueQuestionEventText :: EventField (Maybe String)
  , _editValueQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editValueQuestionEventTagUuids :: EventField [U.UUID]
  , _editValueQuestionEventExpertUuids :: EventField [U.UUID]
  , _editValueQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editValueQuestionEventValueType :: EventField QuestionValueType
  } deriving (Show, Eq, Generic)

data DeleteValueQuestionEvent = DeleteValueQuestionEvent
  { _deleteValueQuestionEventUuid :: U.UUID
  , _deleteValueQuestionEventPath :: EventPath
  , _deleteValueQuestionEventQuestionUuid :: U.UUID
  } deriving (Show, Eq, Generic)
