module Model.Event.Integration.IntegrationEvent where

import Data.Map (Map)
import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddIntegrationEvent = AddIntegrationEvent
  { _addIntegrationEventUuid :: U.UUID
  , _addIntegrationEventPath :: EventPath
  , _addIntegrationEventIntegrationUuid :: U.UUID
  , _addIntegrationEventIId :: String
  , _addIntegrationEventName :: String
  , _addIntegrationEventProps :: [String]
  , _addIntegrationEventLogo :: String
  , _addIntegrationEventRequestMethod :: String
  , _addIntegrationEventRequestUrl :: String
  , _addIntegrationEventRequestHeaders :: Map String String
  , _addIntegrationEventRequestBody :: String
  , _addIntegrationEventResponseListField :: String
  , _addIntegrationEventResponseIdField :: String
  , _addIntegrationEventResponseNameField :: String
  , _addIntegrationEventItemUrl :: String
  } deriving (Show, Eq, Generic)

data EditIntegrationEvent = EditIntegrationEvent
  { _editIntegrationEventUuid :: U.UUID
  , _editIntegrationEventPath :: EventPath
  , _editIntegrationEventIntegrationUuid :: U.UUID
  , _editIntegrationEventIId :: EventField String
  , _editIntegrationEventName :: EventField String
  , _editIntegrationEventProps :: EventField [String]
  , _editIntegrationEventLogo :: EventField String
  , _editIntegrationEventRequestMethod :: EventField String
  , _editIntegrationEventRequestUrl :: EventField String
  , _editIntegrationEventRequestHeaders :: EventField (Map String String)
  , _editIntegrationEventRequestBody :: EventField String
  , _editIntegrationEventResponseListField :: EventField String
  , _editIntegrationEventResponseIdField :: EventField String
  , _editIntegrationEventResponseNameField :: EventField String
  , _editIntegrationEventItemUrl :: EventField String
  } deriving (Show, Eq, Generic)

data DeleteIntegrationEvent = DeleteIntegrationEvent
  { _deleteIntegrationEventUuid :: U.UUID
  , _deleteIntegrationEventPath :: EventPath
  , _deleteIntegrationEventIntegrationUuid :: U.UUID
  } deriving (Show, Eq, Generic)
