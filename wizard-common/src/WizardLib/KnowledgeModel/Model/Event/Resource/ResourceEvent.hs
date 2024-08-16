module WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddResourceCollectionEvent = AddResourceCollectionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data EditResourceCollectionEvent = EditResourceCollectionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , resourcePageUuids :: EventField [U.UUID]
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DeleteResourceCollectionEvent = DeleteResourceCollectionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data AddResourcePageEvent = AddResourcePageEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , content :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data EditResourcePageEvent = EditResourcePageEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , content :: EventField String
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DeleteResourcePageEvent = DeleteResourcePageEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
