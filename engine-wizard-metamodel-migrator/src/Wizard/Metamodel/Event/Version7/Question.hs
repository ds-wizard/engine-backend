module Wizard.Metamodel.Event.Version7.Question where

import Control.Monad
import Data.Aeson
import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version7.Common

-- Shared.Model.Event.Question.QuestionEvent
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
    , _addOptionsQuestionEventRequiredLevel :: Maybe Int
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
    , _addMultiChoiceQuestionEventRequiredLevel :: Maybe Int
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
    , _addListQuestionEventRequiredLevel :: Maybe Int
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
    , _addValueQuestionEventRequiredLevel :: Maybe Int
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
    , _addIntegrationQuestionEventRequiredLevel :: Maybe Int
    , _addIntegrationQuestionEventTagUuids :: [U.UUID]
    , _addIntegrationQuestionEventIntegrationUuid :: U.UUID
    , _addIntegrationQuestionEventProps :: Map String String
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
    , _editOptionsQuestionEventRequiredLevel :: EventField (Maybe Int)
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
    , _editMultiChoiceQuestionEventRequiredLevel :: EventField (Maybe Int)
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
    , _editListQuestionEventRequiredLevel :: EventField (Maybe Int)
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
    , _editValueQuestionEventRequiredLevel :: EventField (Maybe Int)
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
    , _editIntegrationQuestionEventRequiredLevel :: EventField (Maybe Int)
    , _editIntegrationQuestionEventTagUuids :: EventField [U.UUID]
    , _editIntegrationQuestionEventExpertUuids :: EventField [U.UUID]
    , _editIntegrationQuestionEventReferenceUuids :: EventField [U.UUID]
    , _editIntegrationQuestionEventIntegrationUuid :: EventField U.UUID
    , _editIntegrationQuestionEventProps :: EventField (Map String String)
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

-- Shared.Api.Resource.Event.QuestionEventJM
instance ToJSON AddQuestionEvent where
  toJSON = toSumJSON

instance FromJSON AddQuestionEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "questionType"
    case referenceType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEvent' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (AddMultiChoiceQuestionEvent' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEvent' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEvent' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (AddIntegrationQuestionEvent' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddOptionsQuestionEvent where
  parseJSON = simpleParseJSON "_addOptionsQuestionEvent"

instance ToJSON AddOptionsQuestionEvent where
  toJSON = simpleToJSON'' "_addOptionsQuestionEvent" [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON AddMultiChoiceQuestionEvent where
  parseJSON = simpleParseJSON "_addMultiChoiceQuestionEvent"

instance ToJSON AddMultiChoiceQuestionEvent where
  toJSON = simpleToJSON'' "_addMultiChoiceQuestionEvent" [("questionType", "MultiChoiceQuestion")]

-- --------------------------------------------
instance FromJSON AddListQuestionEvent where
  parseJSON = simpleParseJSON "_addListQuestionEvent"

instance ToJSON AddListQuestionEvent where
  toJSON = simpleToJSON'' "_addListQuestionEvent" [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON AddValueQuestionEvent where
  parseJSON = simpleParseJSON "_addValueQuestionEvent"

instance ToJSON AddValueQuestionEvent where
  toJSON = simpleToJSON'' "_addValueQuestionEvent" [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEvent where
  parseJSON = simpleParseJSON "_addIntegrationQuestionEvent"

instance ToJSON AddIntegrationQuestionEvent where
  toJSON = simpleToJSON'' "_addIntegrationQuestionEvent" [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditQuestionEvent where
  toJSON = toSumJSON

instance FromJSON EditQuestionEvent where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEvent' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (EditMultiChoiceQuestionEvent' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEvent' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEvent' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (EditIntegrationQuestionEvent' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditOptionsQuestionEvent where
  parseJSON = simpleParseJSON "_editOptionsQuestionEvent"

instance ToJSON EditOptionsQuestionEvent where
  toJSON = simpleToJSON'' "_editOptionsQuestionEvent" [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON EditMultiChoiceQuestionEvent where
  parseJSON = simpleParseJSON "_editMultiChoiceQuestionEvent"

instance ToJSON EditMultiChoiceQuestionEvent where
  toJSON = simpleToJSON'' "_editMultiChoiceQuestionEvent" [("questionType", "MultiChoiceQuestion")]

-- --------------------------------------------
instance FromJSON EditListQuestionEvent where
  parseJSON = simpleParseJSON "_editListQuestionEvent"

instance ToJSON EditListQuestionEvent where
  toJSON = simpleToJSON'' "_editListQuestionEvent" [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON EditValueQuestionEvent where
  parseJSON = simpleParseJSON "_editValueQuestionEvent"

instance ToJSON EditValueQuestionEvent where
  toJSON = simpleToJSON'' "_editValueQuestionEvent" [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEvent where
  parseJSON = simpleParseJSON "_editIntegrationQuestionEvent"

instance ToJSON EditIntegrationQuestionEvent where
  toJSON = simpleToJSON'' "_editIntegrationQuestionEvent" [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteQuestionEvent where
  parseJSON = simpleParseJSON "_deleteQuestionEvent"

instance ToJSON DeleteQuestionEvent where
  toJSON = simpleToJSON' "_deleteQuestionEvent" "eventType"
