module Shared.Model.Event.Metric.MetricEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

data AddMetricEvent =
  AddMetricEvent
    { _addMetricEventUuid :: U.UUID
    , _addMetricEventParentUuid :: U.UUID
    , _addMetricEventEntityUuid :: U.UUID
    , _addMetricEventTitle :: String
    , _addMetricEventAbbreviation :: Maybe String
    , _addMetricEventDescription :: Maybe String
    , _addMetricEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data EditMetricEvent =
  EditMetricEvent
    { _editMetricEventUuid :: U.UUID
    , _editMetricEventParentUuid :: U.UUID
    , _editMetricEventEntityUuid :: U.UUID
    , _editMetricEventTitle :: EventField String
    , _editMetricEventAbbreviation :: EventField (Maybe String)
    , _editMetricEventDescription :: EventField (Maybe String)
    , _editMetricEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data DeleteMetricEvent =
  DeleteMetricEvent
    { _deleteMetricEventUuid :: U.UUID
    , _deleteMetricEventParentUuid :: U.UUID
    , _deleteMetricEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
