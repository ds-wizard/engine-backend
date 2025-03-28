module WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data AddAnswerEvent = AddAnswerEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , aLabel :: String
  , advice :: Maybe String
  , annotations :: [MapEntry String String]
  , metricMeasures :: [MetricMeasure]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddAnswerEvent

data EditAnswerEvent = EditAnswerEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , aLabel :: EventField String
  , advice :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  , followUpUuids :: EventField [U.UUID]
  , metricMeasures :: EventField [MetricMeasure]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditAnswerEvent

data DeleteAnswerEvent = DeleteAnswerEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq DeleteAnswerEvent where
  a == b =
    a.uuid == b.uuid
      && a.parentUuid == b.parentUuid
      && a.entityUuid == b.entityUuid

instance Hashable DeleteAnswerEvent
