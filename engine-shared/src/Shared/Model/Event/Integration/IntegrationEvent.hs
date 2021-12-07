module Shared.Model.Event.Integration.IntegrationEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

data AddIntegrationEvent =
  AddIntegrationEvent
    { _addIntegrationEventUuid :: U.UUID
    , _addIntegrationEventParentUuid :: U.UUID
    , _addIntegrationEventEntityUuid :: U.UUID
    , _addIntegrationEventIId :: String
    , _addIntegrationEventName :: String
    , _addIntegrationEventProps :: [String]
    , _addIntegrationEventLogo :: String
    , _addIntegrationEventRequestMethod :: String
    , _addIntegrationEventRequestUrl :: String
    , _addIntegrationEventRequestHeaders :: M.Map String String
    , _addIntegrationEventRequestBody :: String
    , _addIntegrationEventResponseListField :: String
    , _addIntegrationEventResponseItemUrl :: String
    , _addIntegrationEventResponseItemId :: String
    , _addIntegrationEventResponseItemTemplate :: String
    , _addIntegrationEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data EditIntegrationEvent =
  EditIntegrationEvent
    { _editIntegrationEventUuid :: U.UUID
    , _editIntegrationEventParentUuid :: U.UUID
    , _editIntegrationEventEntityUuid :: U.UUID
    , _editIntegrationEventIId :: EventField String
    , _editIntegrationEventName :: EventField String
    , _editIntegrationEventProps :: EventField [String]
    , _editIntegrationEventLogo :: EventField String
    , _editIntegrationEventRequestMethod :: EventField String
    , _editIntegrationEventRequestUrl :: EventField String
    , _editIntegrationEventRequestHeaders :: EventField (M.Map String String)
    , _editIntegrationEventRequestBody :: EventField String
    , _editIntegrationEventResponseListField :: EventField String
    , _editIntegrationEventResponseItemUrl :: EventField String
    , _editIntegrationEventResponseItemId :: EventField String
    , _editIntegrationEventResponseItemTemplate :: EventField String
    , _editIntegrationEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data DeleteIntegrationEvent =
  DeleteIntegrationEvent
    { _deleteIntegrationEventUuid :: U.UUID
    , _deleteIntegrationEventParentUuid :: U.UUID
    , _deleteIntegrationEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
