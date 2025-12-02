module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddReferenceEvent
  = AddResourcePageReferenceEvent' AddResourcePageReferenceEvent
  | AddURLReferenceEvent' AddURLReferenceEvent
  | AddCrossReferenceEvent' AddCrossReferenceEvent
  deriving (Show, Eq, Generic)

instance Hashable AddReferenceEvent

data AddResourcePageReferenceEvent = AddResourcePageReferenceEvent
  { resourcePageUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddResourcePageReferenceEvent

data AddURLReferenceEvent = AddURLReferenceEvent
  { url :: String
  , aLabel :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddURLReferenceEvent

data AddCrossReferenceEvent = AddCrossReferenceEvent
  { targetUuid :: U.UUID
  , description :: String
  , annotations :: [MapEntry String String]
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
  { resourcePageUuid :: EventField (Maybe U.UUID)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditResourcePageReferenceEvent

data EditURLReferenceEvent = EditURLReferenceEvent
  { url :: EventField String
  , aLabel :: EventField String
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditURLReferenceEvent

data EditCrossReferenceEvent = EditCrossReferenceEvent
  { targetUuid :: EventField U.UUID
  , description :: EventField String
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditCrossReferenceEvent

-- --------------------------------------------
data DeleteReferenceEvent = DeleteReferenceEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteReferenceEvent
