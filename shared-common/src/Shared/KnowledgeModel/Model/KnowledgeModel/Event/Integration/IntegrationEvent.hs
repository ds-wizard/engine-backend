module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent where

import Data.Hashable
import qualified Data.Map.Strict as M
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Util.Hashable ()

data AddIntegrationEvent
  = AddApiIntegrationEvent' AddApiIntegrationEvent
  | AddApiLegacyIntegrationEvent' AddApiLegacyIntegrationEvent
  | AddWidgetIntegrationEvent' AddWidgetIntegrationEvent
  deriving (Show, Eq, Generic)

instance Hashable AddIntegrationEvent

data AddApiIntegrationEvent = AddApiIntegrationEvent
  { name :: String
  , variables :: [String]
  , allowCustomReply :: Bool
  , requestMethod :: String
  , requestUrl :: String
  , requestHeaders :: [MapEntry String String]
  , requestBody :: Maybe String
  , requestAllowEmptySearch :: Bool
  , responseListField :: Maybe String
  , responseItemTemplate :: String
  , responseItemTemplateForSelection :: Maybe String
  , testQ :: String
  , testVariables :: M.Map String String
  , testResponse :: Maybe TypeHintExchange
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddApiIntegrationEvent

data AddApiLegacyIntegrationEvent = AddApiLegacyIntegrationEvent
  { iId :: String
  , name :: String
  , variables :: [String]
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
  }
  deriving (Show, Eq, Generic)

instance Hashable AddApiLegacyIntegrationEvent

data AddWidgetIntegrationEvent = AddWidgetIntegrationEvent
  { iId :: String
  , name :: String
  , variables :: [String]
  , logo :: Maybe String
  , widgetUrl :: String
  , itemUrl :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddWidgetIntegrationEvent

data EditIntegrationEvent
  = EditApiIntegrationEvent' EditApiIntegrationEvent
  | EditApiLegacyIntegrationEvent' EditApiLegacyIntegrationEvent
  | EditWidgetIntegrationEvent' EditWidgetIntegrationEvent
  deriving (Show, Eq, Generic)

instance Hashable EditIntegrationEvent

data EditApiIntegrationEvent = EditApiIntegrationEvent
  { name :: EventField String
  , variables :: EventField [String]
  , allowCustomReply :: EventField Bool
  , requestMethod :: EventField String
  , requestUrl :: EventField String
  , requestHeaders :: EventField [MapEntry String String]
  , requestBody :: EventField (Maybe String)
  , requestAllowEmptySearch :: EventField Bool
  , responseListField :: EventField (Maybe String)
  , responseItemTemplate :: EventField String
  , responseItemTemplateForSelection :: EventField (Maybe String)
  , testQ :: EventField String
  , testVariables :: EventField (M.Map String String)
  , testResponse :: EventField (Maybe TypeHintExchange)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditApiIntegrationEvent

data EditApiLegacyIntegrationEvent = EditApiLegacyIntegrationEvent
  { iId :: EventField String
  , name :: EventField String
  , variables :: EventField [String]
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
  }
  deriving (Show, Eq, Generic)

instance Hashable EditApiLegacyIntegrationEvent

data EditWidgetIntegrationEvent = EditWidgetIntegrationEvent
  { iId :: EventField String
  , name :: EventField String
  , variables :: EventField [String]
  , logo :: EventField (Maybe String)
  , widgetUrl :: EventField String
  , itemUrl :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditWidgetIntegrationEvent

data DeleteIntegrationEvent = DeleteIntegrationEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteIntegrationEvent

instance Hashable TypeHintExchange

instance Hashable TypeHintRequest

instance Hashable TypeHintResponse

instance Hashable SuccessTypeHintResponse

instance Hashable RemoteErrorTypeHintResponse

instance Hashable RequestFailedTypeHintResponse
