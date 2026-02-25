module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent where

import qualified Data.Aeson as A
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Util.Hashable ()

data AddIntegrationEvent
  = AddApiIntegrationEvent' AddApiIntegrationEvent
  | AddPluginIntegrationEvent' AddPluginIntegrationEvent
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

data AddPluginIntegrationEvent = AddPluginIntegrationEvent
  { pluginUuid :: U.UUID
  , pluginIntegrationId :: String
  , pluginIntegrationSettings :: A.Value
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddPluginIntegrationEvent

data EditIntegrationEvent
  = EditApiIntegrationEvent' EditApiIntegrationEvent
  | EditPluginIntegrationEvent' EditPluginIntegrationEvent
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

data EditPluginIntegrationEvent = EditPluginIntegrationEvent
  { pluginUuid :: EventField U.UUID
  , pluginIntegrationId :: EventField String
  , pluginIntegrationSettings :: EventField A.Value
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditPluginIntegrationEvent

data DeleteIntegrationEvent = DeleteIntegrationEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteIntegrationEvent

instance Hashable TypeHintExchange

instance Hashable TypeHintRequest

instance Hashable TypeHintResponse

instance Hashable SuccessTypeHintResponse

instance Hashable RemoteErrorTypeHintResponse

instance Hashable RequestFailedTypeHintResponse
