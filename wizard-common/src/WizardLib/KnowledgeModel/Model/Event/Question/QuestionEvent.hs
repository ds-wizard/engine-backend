module WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent where

import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddOptionsQuestionEvent

data AddMultiChoiceQuestionEvent = AddMultiChoiceQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddMultiChoiceQuestionEvent

data AddListQuestionEvent = AddListQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddListQuestionEvent

data AddValueQuestionEvent = AddValueQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , valueType :: QuestionValueType
  , validations :: [QuestionValidation]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddValueQuestionEvent

data AddIntegrationQuestionEvent = AddIntegrationQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , integrationUuid :: U.UUID
  , props :: M.Map String String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddIntegrationQuestionEvent

data AddItemSelectQuestionEvent = AddItemSelectQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , listQuestionUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddItemSelectQuestionEvent

data AddFileQuestionEvent = AddFileQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , maxSize :: Maybe Int
  , fileTypes :: Maybe String
  , createdAt :: UTCTime
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
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , answerUuids :: EventField [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditOptionsQuestionEvent

data EditMultiChoiceQuestionEvent = EditMultiChoiceQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , choiceUuids :: EventField [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditMultiChoiceQuestionEvent

data EditListQuestionEvent = EditListQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , itemTemplateQuestionUuids :: EventField [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditListQuestionEvent

data EditValueQuestionEvent = EditValueQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , valueType :: EventField QuestionValueType
  , validations :: EventField [QuestionValidation]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditValueQuestionEvent

data EditIntegrationQuestionEvent = EditIntegrationQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , integrationUuid :: EventField U.UUID
  , props :: EventField (M.Map String String)
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditIntegrationQuestionEvent

data EditItemSelectQuestionEvent = EditItemSelectQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , listQuestionUuid :: EventField (Maybe U.UUID)
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditItemSelectQuestionEvent

data EditFileQuestionEvent = EditFileQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , requiredPhaseUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , tagUuids :: EventField [U.UUID]
  , expertUuids :: EventField [U.UUID]
  , referenceUuids :: EventField [U.UUID]
  , maxSize :: EventField (Maybe Int)
  , fileTypes :: EventField (Maybe String)
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditFileQuestionEvent

-- --------------------------------------------
data DeleteQuestionEvent = DeleteQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable DeleteQuestionEvent
