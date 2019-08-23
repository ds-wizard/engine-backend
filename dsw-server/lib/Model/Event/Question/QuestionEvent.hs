module Model.Event.Question.QuestionEvent where

import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent
  = AddOptionsQuestionEvent' AddOptionsQuestionEvent
  | AddListQuestionEvent' AddListQuestionEvent
  | AddValueQuestionEvent' AddValueQuestionEvent
  | AddIntegrationQuestionEvent' AddIntegrationQuestionEvent
  deriving (Show, Eq, Generic)

data AddOptionsQuestionEvent = AddOptionsQuestionEvent
  { _addOptionsQuestionEventUuid :: U.UUID
  , _addOptionsQuestionEventParentUuid :: U.UUID
  , _addOptionsQuestionEventEntityUuid :: U.UUID
  , _addOptionsQuestionEventTitle :: String
  , _addOptionsQuestionEventText :: Maybe String
  , _addOptionsQuestionEventRequiredLevel :: Maybe Int
  , _addOptionsQuestionEventTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

data AddListQuestionEvent = AddListQuestionEvent
  { _addListQuestionEventUuid :: U.UUID
  , _addListQuestionEventParentUuid :: U.UUID
  , _addListQuestionEventEntityUuid :: U.UUID
  , _addListQuestionEventTitle :: String
  , _addListQuestionEventText :: Maybe String
  , _addListQuestionEventRequiredLevel :: Maybe Int
  , _addListQuestionEventTagUuids :: [U.UUID]
  , _addListQuestionEventItemTemplateTitle :: String
  } deriving (Show, Eq, Generic)

data AddValueQuestionEvent = AddValueQuestionEvent
  { _addValueQuestionEventUuid :: U.UUID
  , _addValueQuestionEventParentUuid :: U.UUID
  , _addValueQuestionEventEntityUuid :: U.UUID
  , _addValueQuestionEventTitle :: String
  , _addValueQuestionEventText :: Maybe String
  , _addValueQuestionEventRequiredLevel :: Maybe Int
  , _addValueQuestionEventTagUuids :: [U.UUID]
  , _addValueQuestionEventValueType :: QuestionValueType
  } deriving (Show, Eq, Generic)

data AddIntegrationQuestionEvent = AddIntegrationQuestionEvent
  { _addIntegrationQuestionEventUuid :: U.UUID
  , _addIntegrationQuestionEventParentUuid :: U.UUID
  , _addIntegrationQuestionEventEntityUuid :: U.UUID
  , _addIntegrationQuestionEventTitle :: String
  , _addIntegrationQuestionEventText :: Maybe String
  , _addIntegrationQuestionEventRequiredLevel :: Maybe Int
  , _addIntegrationQuestionEventTagUuids :: [U.UUID]
  , _addIntegrationQuestionEventIntegrationUuid :: U.UUID
  , _addIntegrationQuestionEventProps :: Map String String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditQuestionEvent
  = EditOptionsQuestionEvent' EditOptionsQuestionEvent
  | EditListQuestionEvent' EditListQuestionEvent
  | EditValueQuestionEvent' EditValueQuestionEvent
  | EditIntegrationQuestionEvent' EditIntegrationQuestionEvent
  deriving (Show, Eq, Generic)

data EditOptionsQuestionEvent = EditOptionsQuestionEvent
  { _editOptionsQuestionEventUuid :: U.UUID
  , _editOptionsQuestionEventParentUuid :: U.UUID
  , _editOptionsQuestionEventEntityUuid :: U.UUID
  , _editOptionsQuestionEventTitle :: EventField String
  , _editOptionsQuestionEventText :: EventField (Maybe String)
  , _editOptionsQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editOptionsQuestionEventTagUuids :: EventField [U.UUID]
  , _editOptionsQuestionEventExpertUuids :: EventField [U.UUID]
  , _editOptionsQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editOptionsQuestionEventAnswerUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)

data EditListQuestionEvent = EditListQuestionEvent
  { _editListQuestionEventUuid :: U.UUID
  , _editListQuestionEventParentUuid :: U.UUID
  , _editListQuestionEventEntityUuid :: U.UUID
  , _editListQuestionEventTitle :: EventField String
  , _editListQuestionEventText :: EventField (Maybe String)
  , _editListQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editListQuestionEventTagUuids :: EventField [U.UUID]
  , _editListQuestionEventExpertUuids :: EventField [U.UUID]
  , _editListQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editListQuestionEventItemTemplateTitle :: EventField String
  , _editListQuestionEventItemTemplateQuestionUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)

data EditValueQuestionEvent = EditValueQuestionEvent
  { _editValueQuestionEventUuid :: U.UUID
  , _editValueQuestionEventParentUuid :: U.UUID
  , _editValueQuestionEventEntityUuid :: U.UUID
  , _editValueQuestionEventTitle :: EventField String
  , _editValueQuestionEventText :: EventField (Maybe String)
  , _editValueQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editValueQuestionEventTagUuids :: EventField [U.UUID]
  , _editValueQuestionEventExpertUuids :: EventField [U.UUID]
  , _editValueQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editValueQuestionEventValueType :: EventField QuestionValueType
  } deriving (Show, Eq, Generic)

data EditIntegrationQuestionEvent = EditIntegrationQuestionEvent
  { _editIntegrationQuestionEventUuid :: U.UUID
  , _editIntegrationQuestionEventParentUuid :: U.UUID
  , _editIntegrationQuestionEventEntityUuid :: U.UUID
  , _editIntegrationQuestionEventTitle :: EventField String
  , _editIntegrationQuestionEventText :: EventField (Maybe String)
  , _editIntegrationQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editIntegrationQuestionEventTagUuids :: EventField [U.UUID]
  , _editIntegrationQuestionEventExpertUuids :: EventField [U.UUID]
  , _editIntegrationQuestionEventReferenceUuids :: EventField [U.UUID]
  , _editIntegrationQuestionEventIntegrationUuid :: EventField U.UUID
  , _editIntegrationQuestionEventProps :: EventField (Map String String)
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteQuestionEvent = DeleteQuestionEvent
  { _deleteQuestionEventUuid :: U.UUID
  , _deleteQuestionEventParentUuid :: U.UUID
  , _deleteQuestionEventEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
