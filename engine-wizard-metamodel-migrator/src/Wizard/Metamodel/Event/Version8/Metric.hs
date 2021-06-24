module Wizard.Metamodel.Event.Version8.Metric where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version8.Common

-- Shared.Model.Event.Metric.MetricEvent
data AddMetricEvent =
  AddMetricEvent
    { _addMetricEventUuid :: U.UUID
    , _addMetricEventParentUuid :: U.UUID
    , _addMetricEventEntityUuid :: U.UUID
    , _addMetricEventTitle :: String
    , _addMetricEventAbbreviation :: Maybe String
    , _addMetricEventDescription :: Maybe String
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
    }
  deriving (Show, Eq, Generic)

data DeleteMetricEvent =
  DeleteMetricEvent
    { _deleteMetricEventUuid :: U.UUID
    , _deleteMetricEventParentUuid :: U.UUID
    , _deleteMetricEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.MetricEventJM
instance FromJSON AddMetricEvent where
  parseJSON = simpleParseJSON "_addMetricEvent"

instance ToJSON AddMetricEvent where
  toJSON = simpleToJSON' "_addMetricEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditMetricEvent where
  parseJSON = simpleParseJSON "_editMetricEvent"

instance ToJSON EditMetricEvent where
  toJSON = simpleToJSON' "_editMetricEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeleteMetricEvent where
  parseJSON = simpleParseJSON "_deleteMetricEvent"

instance ToJSON DeleteMetricEvent where
  toJSON = simpleToJSON' "_deleteMetricEvent" "eventType"
