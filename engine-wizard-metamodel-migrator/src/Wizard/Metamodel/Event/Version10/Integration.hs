module Wizard.Metamodel.Event.Version10.Integration where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version10.Common

-- Shared.Model.Event.Integration.IntegrationEvent
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
    , _addIntegrationEventResponseExampleJson :: Maybe String
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
    , _editIntegrationEventResponseExampleJson :: EventField (Maybe String)
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

-- Shared.Api.Resource.Event.IntegrationEventJM
instance FromJSON AddIntegrationEvent where
  parseJSON = simpleParseJSON "_addIntegrationEvent"

instance ToJSON AddIntegrationEvent where
  toJSON = simpleToJSON' "_addIntegrationEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditIntegrationEvent where
  parseJSON = simpleParseJSON "_editIntegrationEvent"

instance ToJSON EditIntegrationEvent where
  toJSON = simpleToJSON' "_editIntegrationEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeleteIntegrationEvent where
  parseJSON = simpleParseJSON "_deleteIntegrationEvent"

instance ToJSON DeleteIntegrationEvent where
  toJSON = simpleToJSON' "_deleteIntegrationEvent" "eventType"
