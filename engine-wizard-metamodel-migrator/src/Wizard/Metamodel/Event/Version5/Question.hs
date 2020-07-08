module Wizard.Metamodel.Event.Version5.Question where

import Control.Monad
import Data.Aeson
import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version5.Common


-- QuestionEventDTO
data AddQuestionEventDTO
  = AddOptionsQuestionEventDTO' AddOptionsQuestionEventDTO
  | AddListQuestionEventDTO' AddListQuestionEventDTO
  | AddValueQuestionEventDTO' AddValueQuestionEventDTO
  | AddIntegrationQuestionEventDTO' AddIntegrationQuestionEventDTO
  deriving (Show, Eq, Generic)

data AddOptionsQuestionEventDTO =
  AddOptionsQuestionEventDTO
    { _addOptionsQuestionEventDTOUuid :: U.UUID
    , _addOptionsQuestionEventDTOParentUuid :: U.UUID
    , _addOptionsQuestionEventDTOEntityUuid :: U.UUID
    , _addOptionsQuestionEventDTOTitle :: String
    , _addOptionsQuestionEventDTOText :: Maybe String
    , _addOptionsQuestionEventDTORequiredLevel :: Maybe Int
    , _addOptionsQuestionEventDTOTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data AddListQuestionEventDTO =
  AddListQuestionEventDTO
    { _addListQuestionEventDTOUuid :: U.UUID
    , _addListQuestionEventDTOParentUuid :: U.UUID
    , _addListQuestionEventDTOEntityUuid :: U.UUID
    , _addListQuestionEventDTOTitle :: String
    , _addListQuestionEventDTOText :: Maybe String
    , _addListQuestionEventDTORequiredLevel :: Maybe Int
    , _addListQuestionEventDTOTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data AddValueQuestionEventDTO =
  AddValueQuestionEventDTO
    { _addValueQuestionEventDTOUuid :: U.UUID
    , _addValueQuestionEventDTOParentUuid :: U.UUID
    , _addValueQuestionEventDTOEntityUuid :: U.UUID
    , _addValueQuestionEventDTOTitle :: String
    , _addValueQuestionEventDTOText :: Maybe String
    , _addValueQuestionEventDTORequiredLevel :: Maybe Int
    , _addValueQuestionEventDTOTagUuids :: [U.UUID]
    , _addValueQuestionEventDTOValueType :: QuestionValueType
    }
  deriving (Show, Eq, Generic)

data AddIntegrationQuestionEventDTO =
  AddIntegrationQuestionEventDTO
    { _addIntegrationQuestionEventDTOUuid :: U.UUID
    , _addIntegrationQuestionEventDTOParentUuid :: U.UUID
    , _addIntegrationQuestionEventDTOEntityUuid :: U.UUID
    , _addIntegrationQuestionEventDTOTitle :: String
    , _addIntegrationQuestionEventDTOText :: Maybe String
    , _addIntegrationQuestionEventDTORequiredLevel :: Maybe Int
    , _addIntegrationQuestionEventDTOTagUuids :: [U.UUID]
    , _addIntegrationQuestionEventDTOIntegrationUuid :: U.UUID
    , _addIntegrationQuestionEventDTOProps :: Map String String
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditQuestionEventDTO
  = EditOptionsQuestionEventDTO' EditOptionsQuestionEventDTO
  | EditListQuestionEventDTO' EditListQuestionEventDTO
  | EditValueQuestionEventDTO' EditValueQuestionEventDTO
  | EditIntegrationQuestionEventDTO' EditIntegrationQuestionEventDTO
  deriving (Show, Eq, Generic)

data EditOptionsQuestionEventDTO =
  EditOptionsQuestionEventDTO
    { _editOptionsQuestionEventDTOUuid :: U.UUID
    , _editOptionsQuestionEventDTOParentUuid :: U.UUID
    , _editOptionsQuestionEventDTOEntityUuid :: U.UUID
    , _editOptionsQuestionEventDTOTitle :: EventFieldDTO String
    , _editOptionsQuestionEventDTOText :: EventFieldDTO (Maybe String)
    , _editOptionsQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
    , _editOptionsQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
    , _editOptionsQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
    , _editOptionsQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
    , _editOptionsQuestionEventDTOAnswerUuids :: EventFieldDTO [U.UUID]
    }
  deriving (Show, Eq, Generic)

data EditListQuestionEventDTO =
  EditListQuestionEventDTO
    { _editListQuestionEventDTOUuid :: U.UUID
    , _editListQuestionEventDTOParentUuid :: U.UUID
    , _editListQuestionEventDTOEntityUuid :: U.UUID
    , _editListQuestionEventDTOTitle :: EventFieldDTO String
    , _editListQuestionEventDTOText :: EventFieldDTO (Maybe String)
    , _editListQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
    , _editListQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
    , _editListQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
    , _editListQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
    , _editListQuestionEventDTOItemTemplateQuestionUuids :: EventFieldDTO [U.UUID]
    }
  deriving (Show, Eq, Generic)

data EditValueQuestionEventDTO =
  EditValueQuestionEventDTO
    { _editValueQuestionEventDTOUuid :: U.UUID
    , _editValueQuestionEventDTOParentUuid :: U.UUID
    , _editValueQuestionEventDTOEntityUuid :: U.UUID
    , _editValueQuestionEventDTOTitle :: EventFieldDTO String
    , _editValueQuestionEventDTOText :: EventFieldDTO (Maybe String)
    , _editValueQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
    , _editValueQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
    , _editValueQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
    , _editValueQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
    , _editValueQuestionEventDTOValueType :: EventFieldDTO QuestionValueType
    }
  deriving (Show, Eq, Generic)

data EditIntegrationQuestionEventDTO =
  EditIntegrationQuestionEventDTO
    { _editIntegrationQuestionEventDTOUuid :: U.UUID
    , _editIntegrationQuestionEventDTOParentUuid :: U.UUID
    , _editIntegrationQuestionEventDTOEntityUuid :: U.UUID
    , _editIntegrationQuestionEventDTOTitle :: EventFieldDTO String
    , _editIntegrationQuestionEventDTOText :: EventFieldDTO (Maybe String)
    , _editIntegrationQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
    , _editIntegrationQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
    , _editIntegrationQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
    , _editIntegrationQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
    , _editIntegrationQuestionEventDTOIntegrationUuid :: EventFieldDTO U.UUID
    , _editIntegrationQuestionEventDTOProps :: EventFieldDTO (Map String String)
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteQuestionEventDTO =
  DeleteQuestionEventDTO
    { _deleteQuestionEventDTOUuid :: U.UUID
    , _deleteQuestionEventDTOParentUuid :: U.UUID
    , _deleteQuestionEventDTOEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

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
      , "parentUuid" .= _addOptionsQuestionEventDTOParentUuid
      , "entityUuid" .= _addOptionsQuestionEventDTOEntityUuid
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
      , "parentUuid" .= _addListQuestionEventDTOParentUuid
      , "entityUuid" .= _addListQuestionEventDTOEntityUuid
      , "title" .= _addListQuestionEventDTOTitle
      , "text" .= _addListQuestionEventDTOText
      , "requiredLevel" .= _addListQuestionEventDTORequiredLevel
      , "tagUuids" .= _addListQuestionEventDTOTagUuids
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
      , "parentUuid" .= _addValueQuestionEventDTOParentUuid
      , "entityUuid" .= _addValueQuestionEventDTOEntityUuid
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
      , "parentUuid" .= _addIntegrationQuestionEventDTOParentUuid
      , "entityUuid" .= _addIntegrationQuestionEventDTOEntityUuid
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
      , "parentUuid" .= _editOptionsQuestionEventDTOParentUuid
      , "entityUuid" .= _editOptionsQuestionEventDTOEntityUuid
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
      , "parentUuid" .= _editListQuestionEventDTOParentUuid
      , "entityUuid" .= _editListQuestionEventDTOEntityUuid
      , "title" .= _editListQuestionEventDTOTitle
      , "text" .= _editListQuestionEventDTOText
      , "requiredLevel" .= _editListQuestionEventDTORequiredLevel
      , "tagUuids" .= _editListQuestionEventDTOTagUuids
      , "expertUuids" .= _editListQuestionEventDTOExpertUuids
      , "referenceUuids" .= _editListQuestionEventDTOReferenceUuids
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
      , "parentUuid" .= _editValueQuestionEventDTOParentUuid
      , "entityUuid" .= _editValueQuestionEventDTOEntityUuid
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
      , "parentUuid" .= _editIntegrationQuestionEventDTOParentUuid
      , "entityUuid" .= _editIntegrationQuestionEventDTOEntityUuid
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
