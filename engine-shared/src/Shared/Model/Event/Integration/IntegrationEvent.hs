module Shared.Model.Event.Integration.IntegrationEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddIntegrationEvent
  = AddApiIntegrationEvent' AddApiIntegrationEvent
  | AddWidgetIntegrationEvent' AddWidgetIntegrationEvent
  deriving (Show, Eq, Generic)

data AddApiIntegrationEvent =
  AddApiIntegrationEvent
    { _addApiIntegrationEventUuid :: U.UUID
    , _addApiIntegrationEventParentUuid :: U.UUID
    , _addApiIntegrationEventEntityUuid :: U.UUID
    , _addApiIntegrationEventIId :: String
    , _addApiIntegrationEventName :: String
    , _addApiIntegrationEventProps :: [String]
    , _addApiIntegrationEventLogo :: String
    , _addApiIntegrationEventRequestMethod :: String
    , _addApiIntegrationEventRequestUrl :: String
    , _addApiIntegrationEventRequestHeaders :: [MapEntry String String]
    , _addApiIntegrationEventRequestBody :: String
    , _addApiIntegrationEventRequestEmptySearch :: Bool
    , _addApiIntegrationEventResponseListField :: String
    , _addApiIntegrationEventResponseItemId :: String
    , _addApiIntegrationEventResponseItemTemplate :: String
    , _addApiIntegrationEventItemUrl :: String
    , _addApiIntegrationEventAnnotations :: [MapEntry String String]
    , _addApiIntegrationEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data AddWidgetIntegrationEvent =
  AddWidgetIntegrationEvent
    { _addWidgetIntegrationEventUuid :: U.UUID
    , _addWidgetIntegrationEventParentUuid :: U.UUID
    , _addWidgetIntegrationEventEntityUuid :: U.UUID
    , _addWidgetIntegrationEventIId :: String
    , _addWidgetIntegrationEventName :: String
    , _addWidgetIntegrationEventProps :: [String]
    , _addWidgetIntegrationEventLogo :: String
    , _addWidgetIntegrationEventWidgetUrl :: String
    , _addWidgetIntegrationEventItemUrl :: String
    , _addWidgetIntegrationEventAnnotations :: [MapEntry String String]
    , _addWidgetIntegrationEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditIntegrationEvent
  = EditApiIntegrationEvent' EditApiIntegrationEvent
  | EditWidgetIntegrationEvent' EditWidgetIntegrationEvent
  deriving (Show, Eq, Generic)

data EditApiIntegrationEvent =
  EditApiIntegrationEvent
    { _editApiIntegrationEventUuid :: U.UUID
    , _editApiIntegrationEventParentUuid :: U.UUID
    , _editApiIntegrationEventEntityUuid :: U.UUID
    , _editApiIntegrationEventIId :: EventField String
    , _editApiIntegrationEventName :: EventField String
    , _editApiIntegrationEventProps :: EventField [String]
    , _editApiIntegrationEventLogo :: EventField String
    , _editApiIntegrationEventRequestMethod :: EventField String
    , _editApiIntegrationEventRequestUrl :: EventField String
    , _editApiIntegrationEventRequestHeaders :: EventField [MapEntry String String]
    , _editApiIntegrationEventRequestBody :: EventField String
    , _editApiIntegrationEventRequestEmptySearch :: EventField Bool
    , _editApiIntegrationEventResponseListField :: EventField String
    , _editApiIntegrationEventResponseItemId :: EventField String
    , _editApiIntegrationEventResponseItemTemplate :: EventField String
    , _editApiIntegrationEventItemUrl :: EventField String
    , _editApiIntegrationEventAnnotations :: EventField [MapEntry String String]
    , _editApiIntegrationEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditWidgetIntegrationEvent =
  EditWidgetIntegrationEvent
    { _editWidgetIntegrationEventUuid :: U.UUID
    , _editWidgetIntegrationEventParentUuid :: U.UUID
    , _editWidgetIntegrationEventEntityUuid :: U.UUID
    , _editWidgetIntegrationEventIId :: EventField String
    , _editWidgetIntegrationEventName :: EventField String
    , _editWidgetIntegrationEventProps :: EventField [String]
    , _editWidgetIntegrationEventLogo :: EventField String
    , _editWidgetIntegrationEventWidgetUrl :: EventField String
    , _editWidgetIntegrationEventItemUrl :: EventField String
    , _editWidgetIntegrationEventAnnotations :: EventField [MapEntry String String]
    , _editWidgetIntegrationEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data DeleteIntegrationEvent =
  DeleteIntegrationEvent
    { _deleteIntegrationEventUuid :: U.UUID
    , _deleteIntegrationEventParentUuid :: U.UUID
    , _deleteIntegrationEventEntityUuid :: U.UUID
    , _deleteIntegrationEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
