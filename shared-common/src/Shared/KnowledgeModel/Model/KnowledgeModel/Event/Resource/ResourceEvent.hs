module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddResourceCollectionEvent = AddResourceCollectionEvent
  { title :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddResourceCollectionEvent

data EditResourceCollectionEvent = EditResourceCollectionEvent
  { title :: EventField String
  , resourcePageUuids :: EventField [U.UUID]
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditResourceCollectionEvent

data DeleteResourceCollectionEvent = DeleteResourceCollectionEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteResourceCollectionEvent

-- ------------------------------------------------
data AddResourcePageEvent = AddResourcePageEvent
  { title :: String
  , content :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddResourcePageEvent

data EditResourcePageEvent = EditResourcePageEvent
  { title :: EventField String
  , content :: EventField String
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditResourcePageEvent

data DeleteResourcePageEvent = DeleteResourcePageEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteResourcePageEvent
