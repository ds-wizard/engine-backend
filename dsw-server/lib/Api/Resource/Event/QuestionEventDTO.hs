module Api.Resource.Event.QuestionEventDTO where

import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEventDTO
  = AddOptionsQuestionEventDTO' AddOptionsQuestionEventDTO
  | AddListQuestionEventDTO' AddListQuestionEventDTO
  | AddValueQuestionEventDTO' AddValueQuestionEventDTO
  | AddIntegrationQuestionEventDTO' AddIntegrationQuestionEventDTO
  deriving (Show, Eq, Generic)

data AddOptionsQuestionEventDTO = AddOptionsQuestionEventDTO
  { _addOptionsQuestionEventDTOUuid :: U.UUID
  , _addOptionsQuestionEventDTOParentUuid :: U.UUID
  , _addOptionsQuestionEventDTOEntityUuid :: U.UUID
  , _addOptionsQuestionEventDTOTitle :: String
  , _addOptionsQuestionEventDTOText :: Maybe String
  , _addOptionsQuestionEventDTORequiredLevel :: Maybe Int
  , _addOptionsQuestionEventDTOTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

data AddListQuestionEventDTO = AddListQuestionEventDTO
  { _addListQuestionEventDTOUuid :: U.UUID
  , _addListQuestionEventDTOParentUuid :: U.UUID
  , _addListQuestionEventDTOEntityUuid :: U.UUID
  , _addListQuestionEventDTOTitle :: String
  , _addListQuestionEventDTOText :: Maybe String
  , _addListQuestionEventDTORequiredLevel :: Maybe Int
  , _addListQuestionEventDTOTagUuids :: [U.UUID]
  , _addListQuestionEventDTOItemTemplateTitle :: String
  } deriving (Show, Eq, Generic)

data AddValueQuestionEventDTO = AddValueQuestionEventDTO
  { _addValueQuestionEventDTOUuid :: U.UUID
  , _addValueQuestionEventDTOParentUuid :: U.UUID
  , _addValueQuestionEventDTOEntityUuid :: U.UUID
  , _addValueQuestionEventDTOTitle :: String
  , _addValueQuestionEventDTOText :: Maybe String
  , _addValueQuestionEventDTORequiredLevel :: Maybe Int
  , _addValueQuestionEventDTOTagUuids :: [U.UUID]
  , _addValueQuestionEventDTOValueType :: QuestionValueType
  } deriving (Show, Eq, Generic)

data AddIntegrationQuestionEventDTO = AddIntegrationQuestionEventDTO
  { _addIntegrationQuestionEventDTOUuid :: U.UUID
  , _addIntegrationQuestionEventDTOParentUuid :: U.UUID
  , _addIntegrationQuestionEventDTOEntityUuid :: U.UUID
  , _addIntegrationQuestionEventDTOTitle :: String
  , _addIntegrationQuestionEventDTOText :: Maybe String
  , _addIntegrationQuestionEventDTORequiredLevel :: Maybe Int
  , _addIntegrationQuestionEventDTOTagUuids :: [U.UUID]
  , _addIntegrationQuestionEventDTOIntegrationUuid :: U.UUID
  , _addIntegrationQuestionEventDTOProps :: Map String String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditQuestionEventDTO
  = EditOptionsQuestionEventDTO' EditOptionsQuestionEventDTO
  | EditListQuestionEventDTO' EditListQuestionEventDTO
  | EditValueQuestionEventDTO' EditValueQuestionEventDTO
  | EditIntegrationQuestionEventDTO' EditIntegrationQuestionEventDTO
  deriving (Show, Eq, Generic)

data EditOptionsQuestionEventDTO = EditOptionsQuestionEventDTO
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
  } deriving (Show, Eq, Generic)

data EditListQuestionEventDTO = EditListQuestionEventDTO
  { _editListQuestionEventDTOUuid :: U.UUID
  , _editListQuestionEventDTOParentUuid :: U.UUID
  , _editListQuestionEventDTOEntityUuid :: U.UUID
  , _editListQuestionEventDTOTitle :: EventFieldDTO String
  , _editListQuestionEventDTOText :: EventFieldDTO (Maybe String)
  , _editListQuestionEventDTORequiredLevel :: EventFieldDTO (Maybe Int)
  , _editListQuestionEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editListQuestionEventDTOExpertUuids :: EventFieldDTO [U.UUID]
  , _editListQuestionEventDTOReferenceUuids :: EventFieldDTO [U.UUID]
  , _editListQuestionEventDTOItemTemplateTitle :: EventFieldDTO String
  , _editListQuestionEventDTOItemTemplateQuestionUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data EditValueQuestionEventDTO = EditValueQuestionEventDTO
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
  } deriving (Show, Eq, Generic)

data EditIntegrationQuestionEventDTO = EditIntegrationQuestionEventDTO
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
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteQuestionEventDTO = DeleteQuestionEventDTO
  { _deleteQuestionEventDTOUuid :: U.UUID
  , _deleteQuestionEventDTOParentUuid :: U.UUID
  , _deleteQuestionEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
