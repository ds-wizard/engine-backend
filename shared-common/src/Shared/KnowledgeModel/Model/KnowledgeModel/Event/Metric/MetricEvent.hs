module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent where

import Data.Hashable
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddMetricEvent = AddMetricEvent
  { title :: String
  , abbreviation :: Maybe String
  , description :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddMetricEvent

data EditMetricEvent = EditMetricEvent
  { title :: EventField String
  , abbreviation :: EventField (Maybe String)
  , description :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditMetricEvent

data DeleteMetricEvent = DeleteMetricEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteMetricEvent
