module WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddIntegrationEvent
  = AddApiLegacyIntegrationEvent' AddApiLegacyIntegrationEvent
  | AddWidgetIntegrationEvent' AddWidgetIntegrationEvent
  deriving (Show, Eq, Generic)

instance Hashable AddIntegrationEvent

data AddApiLegacyIntegrationEvent = AddApiLegacyIntegrationEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , iId :: String
  , name :: String
  , props :: [String]
  , logo :: Maybe String
  , requestMethod :: String
  , requestUrl :: String
  , requestHeaders :: [MapEntry String String]
  , requestBody :: String
  , requestEmptySearch :: Bool
  , responseListField :: Maybe String
  , responseItemId :: Maybe String
  , responseItemTemplate :: String
  , itemUrl :: Maybe String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddApiLegacyIntegrationEvent

data AddWidgetIntegrationEvent = AddWidgetIntegrationEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , iId :: String
  , name :: String
  , props :: [String]
  , logo :: Maybe String
  , widgetUrl :: String
  , itemUrl :: Maybe String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddWidgetIntegrationEvent

data EditIntegrationEvent
  = EditApiLegacyIntegrationEvent' EditApiLegacyIntegrationEvent
  | EditWidgetIntegrationEvent' EditWidgetIntegrationEvent
  deriving (Show, Eq, Generic)

instance Hashable EditIntegrationEvent

data EditApiLegacyIntegrationEvent = EditApiLegacyIntegrationEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , iId :: EventField String
  , name :: EventField String
  , props :: EventField [String]
  , logo :: EventField (Maybe String)
  , requestMethod :: EventField String
  , requestUrl :: EventField String
  , requestHeaders :: EventField [MapEntry String String]
  , requestBody :: EventField String
  , requestEmptySearch :: EventField Bool
  , responseListField :: EventField (Maybe String)
  , responseItemId :: EventField (Maybe String)
  , responseItemTemplate :: EventField String
  , itemUrl :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditApiLegacyIntegrationEvent

data EditWidgetIntegrationEvent = EditWidgetIntegrationEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , iId :: EventField String
  , name :: EventField String
  , props :: EventField [String]
  , logo :: EventField (Maybe String)
  , widgetUrl :: EventField String
  , itemUrl :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditWidgetIntegrationEvent

data DeleteIntegrationEvent = DeleteIntegrationEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable DeleteIntegrationEvent
