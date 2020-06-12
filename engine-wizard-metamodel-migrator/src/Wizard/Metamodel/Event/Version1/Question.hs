module Wizard.Metamodel.Event.Version1.Question where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version1.Common


data AddQuestionEventDTO
  = AddOptionsQuestionEventDTO' AddOptionsQuestionEventDTO
  | AddListQuestionEventDTO' AddListQuestionEventDTO
  | AddValueQuestionEventDTO' AddValueQuestionEventDTO
  deriving (Show, Eq, Generic)

data AddOptionsQuestionEventDTO =
  AddOptionsQuestionEventDTO
    { _addOptionsQuestionEventDTOUuid :: U.UUID
    , _addOptionsQuestionEventDTOPath :: EventPathDTO
    , _addOptionsQuestionEventDTOQuestionUuid :: U.UUID
    , _addOptionsQuestionEventDTOTitle :: String
    , _addOptionsQuestionEventDTOText :: Maybe String
    , _addOptionsQuestionEventDTORequiredLevel :: Maybe Int
    , _addOptionsQuestionEventDTOTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data AddListQuestionEventDTO =
  AddListQuestionEventDTO
    { _addListQuestionEventDTOUuid :: U.UUID
    , _addListQuestionEventDTOPath :: EventPathDTO
    , _addListQuestionEventDTOQuestionUuid :: U.UUID
    , _addListQuestionEventDTOTitle :: String
    , _addListQuestionEventDTOText :: Maybe String
    , _addListQuestionEventDTORequiredLevel :: Maybe Int
    , _addListQuestionEventDTOTagUuids :: [U.UUID]
    , _addListQuestionEventDTOItemTemplateTitle :: String
    }
  deriving (Show, Eq, Generic)

data AddValueQuestionEventDTO =
  AddValueQuestionEventDTO
    { _addValueQuestionEventDTOUuid :: U.UUID
    , _addValueQuestionEventDTOPath :: EventPathDTO
    , _addValueQuestionEventDTOQuestionUuid :: U.UUID
    , _addValueQuestionEventDTOTitle :: String
    , _addValueQuestionEventDTOText :: Maybe String
    , _addValueQuestionEventDTORequiredLevel :: Maybe Int
    , _addValueQuestionEventDTOTagUuids :: [U.UUID]
    , _addValueQuestionEventDTOValueType :: QuestionValueType
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditQuestionEventDTO
  = EditOptionsQuestionEventDTO' EditOptionsQuestionEventDTO
  | EditListQuestionEventDTO' EditListQuestionEventDTO
  | EditValueQuestionEventDTO' EditValueQuestionEventDTO
  deriving (Show, Eq, Generic)

data EditOptionsQuestionEventDTO =
  EditOptionsQuestionEventDTO
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
    }
  deriving (Show, Eq, Generic)

data EditListQuestionEventDTO =
  EditListQuestionEventDTO
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
    }
  deriving (Show, Eq, Generic)

data EditValueQuestionEventDTO =
  EditValueQuestionEventDTO
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
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteQuestionEventDTO =
  DeleteQuestionEventDTO
    { _deleteQuestionEventDTOUuid :: U.UUID
    , _deleteQuestionEventDTOPath :: EventPathDTO
    , _deleteQuestionEventDTOQuestionUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

instance ToJSON AddQuestionEventDTO where
  toJSON (AddOptionsQuestionEventDTO' event) = toJSON event
  toJSON (AddListQuestionEventDTO' event) = toJSON event
  toJSON (AddValueQuestionEventDTO' event) = toJSON event

instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "questionType"
    case referenceType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEventDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEventDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEventDTO' event)
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
-- --------------------------------------------
instance ToJSON EditQuestionEventDTO where
  toJSON (EditOptionsQuestionEventDTO' event) = toJSON event
  toJSON (EditListQuestionEventDTO' event) = toJSON event
  toJSON (EditValueQuestionEventDTO' event) = toJSON event

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEventDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEventDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEventDTO' event)
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

