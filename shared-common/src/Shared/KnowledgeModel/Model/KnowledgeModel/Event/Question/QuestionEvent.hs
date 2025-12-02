module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent where

import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Util.Hashable ()

data AddQuestionEvent
  = AddOptionsQuestionEvent' AddOptionsQuestionEvent
  | AddMultiChoiceQuestionEvent' AddMultiChoiceQuestionEvent
  | AddListQuestionEvent' AddListQuestionEvent
  | AddValueQuestionEvent' AddValueQuestionEvent
  | AddIntegrationQuestionEvent' AddIntegrationQuestionEvent
  | AddItemSelectQuestionEvent' AddItemSelectQuestionEvent
  | AddFileQuestionEvent' AddFileQuestionEvent
  deriving (Show, Eq, Generic)

instance Hashable AddQuestionEvent

data AddOptionsQuestionEvent = AddOptionsQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddOptionsQuestionEvent

data AddMultiChoiceQuestionEvent = AddMultiChoiceQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddMultiChoiceQuestionEvent

data AddListQuestionEvent = AddListQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddListQuestionEvent

data AddValueQuestionEvent = AddValueQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , valueType :: QuestionValueType
  , validations :: [QuestionValidation]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddValueQuestionEvent

data AddIntegrationQuestionEvent = AddIntegrationQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , integrationUuid :: U.UUID
  , variables :: M.Map String String
  }
  deriving (Show, Eq, Generic)

instance Hashable AddIntegrationQuestionEvent

data AddItemSelectQuestionEvent = AddItemSelectQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , listQuestionUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Hashable AddItemSelectQuestionEvent

data AddFileQuestionEvent = AddFileQuestionEvent
  { title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , maxSize :: Maybe Int
  , fileTypes :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance Hashable AddFileQuestionEvent

-- --------------------------------------------
data EditQuestionEvent
  = EditOptionsQuestionEvent' EditOptionsQuestionEvent
  | EditMultiChoiceQuestionEvent' EditMultiChoiceQuestionEvent
  | EditListQuestionEvent' EditListQuestionEvent
  | EditValueQuestionEvent' EditValueQuestionEvent
  | EditIntegrationQuestionEvent' EditIntegrationQuestionEvent
  | EditItemSelectQuestionEvent' EditItemSelectQuestionEvent
  | EditFileQuestionEvent' EditFileQuestionEvent
  deriving (Show, Eq, Generic)

instance Hashable EditQuestionEvent

data EditOptionsQuestionEvent = EditOptionsQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , answerUuids :: EventField [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditOptionsQuestionEvent

data EditMultiChoiceQuestionEvent = EditMultiChoiceQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , choiceUuids :: EventField [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditMultiChoiceQuestionEvent

data EditListQuestionEvent = EditListQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , itemTemplateQuestionUuids :: EventField [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditListQuestionEvent

data EditValueQuestionEvent = EditValueQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , valueType :: EventField QuestionValueType
  , validations :: EventField [QuestionValidation]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditValueQuestionEvent

data EditIntegrationQuestionEvent = EditIntegrationQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , integrationUuid :: EventField U.UUID
  , variables :: EventField (M.Map String String)
  }
  deriving (Show, Eq, Generic)

instance Hashable EditIntegrationQuestionEvent

data EditItemSelectQuestionEvent = EditItemSelectQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , listQuestionUuid :: EventField (Maybe U.UUID)
  }
  deriving (Show, Eq, Generic)

instance Hashable EditItemSelectQuestionEvent

data EditFileQuestionEvent = EditFileQuestionEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , maxSize :: EventField (Maybe Int)
  , fileTypes :: EventField (Maybe String)
  }
  deriving (Show, Eq, Generic)

instance Hashable EditFileQuestionEvent

-- --------------------------------------------
data DeleteQuestionEvent = DeleteQuestionEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteQuestionEvent
