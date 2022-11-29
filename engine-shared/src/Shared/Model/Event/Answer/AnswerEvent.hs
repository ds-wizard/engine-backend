module Shared.Model.Event.Answer.AnswerEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel

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

data DeleteAnswerEvent = DeleteAnswerEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
