module Shared.Model.Event.Question.QuestionEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent
  = AddOptionsQuestionEvent' AddOptionsQuestionEvent
  | AddMultiChoiceQuestionEvent' AddMultiChoiceQuestionEvent
  | AddListQuestionEvent' AddListQuestionEvent
  | AddValueQuestionEvent' AddValueQuestionEvent
  | AddIntegrationQuestionEvent' AddIntegrationQuestionEvent
  deriving (Show, Eq, Generic)

data AddOptionsQuestionEvent =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid :: U.UUID
    , _addOptionsQuestionEventParentUuid :: U.UUID
    , _addOptionsQuestionEventEntityUuid :: U.UUID
    , _addOptionsQuestionEventTitle :: String
    , _addOptionsQuestionEventText :: Maybe String
    , _addOptionsQuestionEventRequiredPhaseUuid :: Maybe U.UUID
    , _addOptionsQuestionEventAnnotations :: M.Map String String
    , _addOptionsQuestionEventTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data AddMultiChoiceQuestionEvent =
  AddMultiChoiceQuestionEvent
    { _addMultiChoiceQuestionEventUuid :: U.UUID
    , _addMultiChoiceQuestionEventParentUuid :: U.UUID
    , _addMultiChoiceQuestionEventEntityUuid :: U.UUID
    , _addMultiChoiceQuestionEventTitle :: String
    , _addMultiChoiceQuestionEventText :: Maybe String
    , _addMultiChoiceQuestionEventRequiredPhaseUuid :: Maybe U.UUID
    , _addMultiChoiceQuestionEventAnnotations :: M.Map String String
    , _addMultiChoiceQuestionEventTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data AddListQuestionEvent =
  AddListQuestionEvent
    { _addListQuestionEventUuid :: U.UUID
    , _addListQuestionEventParentUuid :: U.UUID
    , _addListQuestionEventEntityUuid :: U.UUID
    , _addListQuestionEventTitle :: String
    , _addListQuestionEventText :: Maybe String
    , _addListQuestionEventRequiredPhaseUuid :: Maybe U.UUID
    , _addListQuestionEventAnnotations :: M.Map String String
    , _addListQuestionEventTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data AddValueQuestionEvent =
  AddValueQuestionEvent
    { _addValueQuestionEventUuid :: U.UUID
    , _addValueQuestionEventParentUuid :: U.UUID
    , _addValueQuestionEventEntityUuid :: U.UUID
    , _addValueQuestionEventTitle :: String
    , _addValueQuestionEventText :: Maybe String
    , _addValueQuestionEventRequiredPhaseUuid :: Maybe U.UUID
    , _addValueQuestionEventAnnotations :: M.Map String String
    , _addValueQuestionEventTagUuids :: [U.UUID]
    , _addValueQuestionEventValueType :: QuestionValueType
    }
  deriving (Show, Eq, Generic)

data AddIntegrationQuestionEvent =
  AddIntegrationQuestionEvent
    { _addIntegrationQuestionEventUuid :: U.UUID
    , _addIntegrationQuestionEventParentUuid :: U.UUID
    , _addIntegrationQuestionEventEntityUuid :: U.UUID
    , _addIntegrationQuestionEventTitle :: String
    , _addIntegrationQuestionEventText :: Maybe String
    , _addIntegrationQuestionEventRequiredPhaseUuid :: Maybe U.UUID
    , _addIntegrationQuestionEventAnnotations :: M.Map String String
    , _addIntegrationQuestionEventTagUuids :: [U.UUID]
    , _addIntegrationQuestionEventIntegrationUuid :: U.UUID
    , _addIntegrationQuestionEventProps :: M.Map String String
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditQuestionEvent
  = EditOptionsQuestionEvent' EditOptionsQuestionEvent
  | EditMultiChoiceQuestionEvent' EditMultiChoiceQuestionEvent
  | EditListQuestionEvent' EditListQuestionEvent
  | EditValueQuestionEvent' EditValueQuestionEvent
  | EditIntegrationQuestionEvent' EditIntegrationQuestionEvent
  deriving (Show, Eq, Generic)

data EditOptionsQuestionEvent =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid :: U.UUID
    , _editOptionsQuestionEventParentUuid :: U.UUID
    , _editOptionsQuestionEventEntityUuid :: U.UUID
    , _editOptionsQuestionEventTitle :: EventField String
    , _editOptionsQuestionEventText :: EventField (Maybe String)
    , _editOptionsQuestionEventRequiredPhaseUuid :: EventField (Maybe U.UUID)
    , _editOptionsQuestionEventAnnotations :: EventField (M.Map String String)
    , _editOptionsQuestionEventTagUuids :: EventField [U.UUID]
    , _editOptionsQuestionEventExpertUuids :: EventField [U.UUID]
    , _editOptionsQuestionEventReferenceUuids :: EventField [U.UUID]
    , _editOptionsQuestionEventAnswerUuids :: EventField [U.UUID]
    }
  deriving (Show, Eq, Generic)

data EditMultiChoiceQuestionEvent =
  EditMultiChoiceQuestionEvent
    { _editMultiChoiceQuestionEventUuid :: U.UUID
    , _editMultiChoiceQuestionEventParentUuid :: U.UUID
    , _editMultiChoiceQuestionEventEntityUuid :: U.UUID
    , _editMultiChoiceQuestionEventTitle :: EventField String
    , _editMultiChoiceQuestionEventText :: EventField (Maybe String)
    , _editMultiChoiceQuestionEventRequiredPhaseUuid :: EventField (Maybe U.UUID)
    , _editMultiChoiceQuestionEventAnnotations :: EventField (M.Map String String)
    , _editMultiChoiceQuestionEventTagUuids :: EventField [U.UUID]
    , _editMultiChoiceQuestionEventExpertUuids :: EventField [U.UUID]
    , _editMultiChoiceQuestionEventReferenceUuids :: EventField [U.UUID]
    , _editMultiChoiceQuestionEventChoiceUuids :: EventField [U.UUID]
    }
  deriving (Show, Eq, Generic)

data EditListQuestionEvent =
  EditListQuestionEvent
    { _editListQuestionEventUuid :: U.UUID
    , _editListQuestionEventParentUuid :: U.UUID
    , _editListQuestionEventEntityUuid :: U.UUID
    , _editListQuestionEventTitle :: EventField String
    , _editListQuestionEventText :: EventField (Maybe String)
    , _editListQuestionEventRequiredPhaseUuid :: EventField (Maybe U.UUID)
    , _editListQuestionEventAnnotations :: EventField (M.Map String String)
    , _editListQuestionEventTagUuids :: EventField [U.UUID]
    , _editListQuestionEventExpertUuids :: EventField [U.UUID]
    , _editListQuestionEventReferenceUuids :: EventField [U.UUID]
    , _editListQuestionEventItemTemplateQuestionUuids :: EventField [U.UUID]
    }
  deriving (Show, Eq, Generic)

data EditValueQuestionEvent =
  EditValueQuestionEvent
    { _editValueQuestionEventUuid :: U.UUID
    , _editValueQuestionEventParentUuid :: U.UUID
    , _editValueQuestionEventEntityUuid :: U.UUID
    , _editValueQuestionEventTitle :: EventField String
    , _editValueQuestionEventText :: EventField (Maybe String)
    , _editValueQuestionEventRequiredPhaseUuid :: EventField (Maybe U.UUID)
    , _editValueQuestionEventAnnotations :: EventField (M.Map String String)
    , _editValueQuestionEventTagUuids :: EventField [U.UUID]
    , _editValueQuestionEventExpertUuids :: EventField [U.UUID]
    , _editValueQuestionEventReferenceUuids :: EventField [U.UUID]
    , _editValueQuestionEventValueType :: EventField QuestionValueType
    }
  deriving (Show, Eq, Generic)

data EditIntegrationQuestionEvent =
  EditIntegrationQuestionEvent
    { _editIntegrationQuestionEventUuid :: U.UUID
    , _editIntegrationQuestionEventParentUuid :: U.UUID
    , _editIntegrationQuestionEventEntityUuid :: U.UUID
    , _editIntegrationQuestionEventTitle :: EventField String
    , _editIntegrationQuestionEventText :: EventField (Maybe String)
    , _editIntegrationQuestionEventRequiredPhaseUuid :: EventField (Maybe U.UUID)
    , _editIntegrationQuestionEventAnnotations :: EventField (M.Map String String)
    , _editIntegrationQuestionEventTagUuids :: EventField [U.UUID]
    , _editIntegrationQuestionEventExpertUuids :: EventField [U.UUID]
    , _editIntegrationQuestionEventReferenceUuids :: EventField [U.UUID]
    , _editIntegrationQuestionEventIntegrationUuid :: EventField U.UUID
    , _editIntegrationQuestionEventProps :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteQuestionEvent =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid :: U.UUID
    , _deleteQuestionEventParentUuid :: U.UUID
    , _deleteQuestionEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
