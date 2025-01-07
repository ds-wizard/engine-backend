module WizardLib.KnowledgeModel.Model.Event.Reference.ReferenceEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddReferenceEvent
  = AddResourcePageReferenceEvent' AddResourcePageReferenceEvent
  | AddURLReferenceEvent' AddURLReferenceEvent
  | AddCrossReferenceEvent' AddCrossReferenceEvent
  deriving (Show, Eq, Generic)

instance Hashable AddReferenceEvent

data AddResourcePageReferenceEvent = AddResourcePageReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , resourcePageUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddResourcePageReferenceEvent

data AddURLReferenceEvent = AddURLReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , url :: String
  , aLabel :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddURLReferenceEvent

data AddCrossReferenceEvent = AddCrossReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , description :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddCrossReferenceEvent

-- --------------------------------------------
data EditReferenceEvent
  = EditResourcePageReferenceEvent' EditResourcePageReferenceEvent
  | EditURLReferenceEvent' EditURLReferenceEvent
  | EditCrossReferenceEvent' EditCrossReferenceEvent
  deriving (Show, Eq, Generic)

instance Hashable EditReferenceEvent

data EditResourcePageReferenceEvent = EditResourcePageReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , resourcePageUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditResourcePageReferenceEvent

data EditURLReferenceEvent = EditURLReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , url :: EventField String
  , aLabel :: EventField String
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditURLReferenceEvent

data EditCrossReferenceEvent = EditCrossReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: EventField U.UUID
  , description :: EventField String
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditCrossReferenceEvent

-- --------------------------------------------
data DeleteReferenceEvent = DeleteReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable DeleteReferenceEvent
